{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.ConnectVerbose (startGUIDriverVerbose, startDriverVerbose) where

import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import qualified Control.Monad.Parallel as P
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad 
import Control.Monad.Loops

import Prelude hiding (concat)
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock
import Data.ByteString (ByteString,concat)
import Data.ByteString.Char8 (pack)
import Data.Tuple
import Data.Maybe
import Data.Either

import System.Exit 
import System.Timeout
import System.Process
import System.Directory

import GHC.IO.Handle

import FRP.Yampa 

import TORCS.Types
import TORCS.Parser

type Cost = Double

-- | collect every input and action during the session (in reverse chrono order (last action to first))
startDriverVerbose :: Driver -> IO([(CarState, DriveState, Cost)])
startDriverVerbose d    = startDriverWithPort False M.empty d 0 "3001" 

startGUIDriverVerbose :: Driver -> IO([(CarState, DriveState, Cost)])
startGUIDriverVerbose d    = startDriverWithPort True M.empty d 0 "3001" 
        
--NB torcs requires full path
startDriverWithPort :: Bool -> M.Map Int (MVar String) -> Driver -> Int -> ServiceName -> IO ([(CarState,DriveState,Cost)])
startDriverWithPort gui mvars myDriver delay port = withSocketsDo $ bracket connectMe close (yampaRunner myDriver mvars port)
  where
    connectMe = do
      homeDir <- getHomeDirectory
      unless gui $ createProcess (proc "torcs" ["-r "++homeDir++"/.torcs/config/raceman/practice.xml"]) {std_out = CreatePipe} >> return ()
      (serveraddr:_) <- getAddrInfo
                          (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                          Nothing (Just port)
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)

      threadDelay delay
      threadDelay 100000
      let mysteryString = concat["SCR",(pack $ show port),"(init -90 -75 -60 -45 -30 -20 -15 -10 -5 0 5 10 15 20 30 45 60 75 90)"] 
      sendTo sock mysteryString (addrAddress serveraddr) 
      return sock

  
--the id (port number) is used to choose this car's writing mvar
yampaRunner :: Driver -> M.Map Int (MVar String) -> ServiceName -> Socket -> IO ([(CarState, DriveState,Cost)])
yampaRunner myDriver allChannels id conn = do
  --let trySend = timeout 10000 $ try (attemptSend) :: IO (Maybe (Either (SomeException) Int))
  t <- getCurrentTime
  timeRef <- newIORef t
  driveRef <- newIORef [defaultDriveState]
  carRef <- newIORef [defaultCarState]
  let myChannel = read id :: Int
  (msg,addr) <- recvFrom conn 1024
  reactimate
    (return NoEvent)
    (sense timeRef conn allChannels carRef)
    (action conn addr (M.lookup myChannel allChannels) driveRef)
    myDriver
  d <- readIORef driveRef
  c <- readIORef carRef
  -- cost is how far you got
  --print c
  cost <- return $ distRaced $ head c
  return $ zip3 c d (repeat cost)

-- action will do two things separately
-- first, send the drive instructions
-- second, broadcast the message to the other threads
action :: Socket -> SockAddr -> Maybe (MVar String) -> IORef [DriveState] ->
          Bool -> DriveState -> IO Bool
action conn d myBroadcastChan outRef _ msg = do
  bytesSent <- sendTo conn (toByteString msg) d 
  oldVal <- maybe (return Nothing) tryReadMVar myBroadcastChan
  _ <- maybe (return False) (\x -> if oldVal == (Just $ broadcast msg) then return False else mySwapMVar x (broadcast msg)) myBroadcastChan
  --if we just sent a restart signal, end the reactimation (return True)
  --otherwise, continue (return False)
  modifyIORef' outRef (msg:)
  --print msg
  if (meta msg == 1)
  then return True 
  else return False

mySwapMVar :: MVar a -> a -> IO Bool
mySwapMVar m v = do
  tryTakeMVar m
  tryPutMVar m v
  
-- sensing will try to read from all the mvars, and add this info to CarState
-- this only writes to the lapTimes part of CarState (not native to TORCS)
sense :: IORef UTCTime -> Socket -> M.Map Int (MVar String) -> IORef [CarState] -> Bool -> IO (DTime, Maybe (Event CarState))
sense timeRef conn chans carRef _ = do
  cur <- getCurrentTime
  (msg,d) <- catch (recvFrom conn 1024) (\(e :: SomeException) -> return ("",SockAddrUnix "")) --if nothing to sense from, get default value
  ms <- mapM tryReadMVar chans :: IO (M.Map Int (Maybe String))
  dt <- timediff timeRef cur
  let rawCarState = (fromByteString msg){communications = ms}
  oldCarState <- readIORef carRef
  let carState = rawCarState{lapTimes = countLaps (lapTimes $ head oldCarState, lastLapTime rawCarState)}
  modifyIORef' carRef (carState:)
  --print $ gear' carState
  --print $ distRaced carState
  return (dt, Just $ return carState) --TODO do i need the Event wrapper?

--A small wrapper to keep track of how long each lap took
countLaps :: ([Double],Double) -> [Double]
countLaps (lapTs,lastT) = if 
  | lastT == 0 -> []
  | lastT>0 && (length lapTs == 0 || lastT /= head lapTs) -> lastT : lapTs
  | otherwise -> lapTs

timediff ::  IORef UTCTime -> UTCTime -> IO DTime
timediff ref cur = do
  old <- readIORef ref
  writeIORef ref cur
  return $ realToFrac $ diffUTCTime cur old
