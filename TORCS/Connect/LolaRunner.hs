{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Connect.LolaRunner (startDriverWithPort) where

import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import Control.Concurrent
import Control.Monad 

import Prelude hiding (concat)
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock
import Data.ByteString (concat)
import Data.ByteString.Char8 (pack)

import System.Process
import System.Directory

import FRP.Yampa 

import TORCS.Types
import TORCS.Parser
import TORCS.Connect.Util

--NB torcs requires full path
startDriverWithPort :: Bool -> M.Map Int (MVar String) -> Driver -> Int -> ServiceName -> IO (CarState,DriveState)
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

  
monitorWrapper :: (CarState,DriveState) -> IO String
monitorWrapper (cs,ds) = do
  print "running the monitor"
  return "monitor output"

--the id (port number) is used to choose this car's writing mvar
yampaRunner :: Driver -> M.Map Int (MVar String) -> ServiceName -> Socket -> IO (CarState, DriveState)
yampaRunner myDriver allChannels id conn = do
  --let trySend = timeout 10000 $ try (attemptSend) :: IO (Maybe (Either (SomeException) Int))
  t <- getCurrentTime
  timeRef <- newIORef t
  driveRef <- newIORef defaultDriveState
  carRef <- newIORef defaultCarState
  let myChannel = read id :: Int
  (msg,addr) <- recvFrom conn 1024
  print "Starting new driver"
  reactimate
    (return NoEvent)
    (sense timeRef conn allChannels carRef driveRef monitorWrapper)
    (action conn addr (M.lookup myChannel allChannels) driveRef)
    myDriver
  --TODO clean this syntax
  d <- readIORef driveRef
  c <- readIORef carRef
  return (c,d)

-- action will do two things separately
-- first, send the drive instructions
-- second, broadcast the message to the other threads
action :: Socket -> SockAddr -> Maybe (MVar String) -> IORef DriveState ->
          Bool -> DriveState -> IO Bool
action conn d myBroadcastChan outRef _ msg = do
  bytesSent <- sendTo conn (toByteString msg) d 
  oldVal <- maybe (return Nothing) tryReadMVar myBroadcastChan
  _ <- maybe (return False) (\x -> if oldVal == (Just $ broadcast msg) then return False else mySwapMVar x (broadcast msg)) myBroadcastChan
  --since our monitor needs updated DriveState in sense, we update the ref everytime now
  writeIORef outRef msg
  --if we just sent a restart signal, end the reactimation (return True)
  --otherwise, continue (return False)
  if (meta msg == 1)
  then return True 
  else return False
  
-- sensing will try to read from all the mvars, and add this info to CarState
-- this only writes to the lapTimes part of CarState (not native to TORCS)
sense :: IORef UTCTime -> Socket -> M.Map Int (MVar String) -> IORef CarState -> IORef DriveState -> ((CarState,DriveState) -> IO String) -> Bool -> IO (DTime, Maybe (Event CarState))
sense timeRef conn chans carRef driveRef monitorAction _ = do
  cur <- getCurrentTime
  (msg,d) <- catch (recvFrom conn 1024) (\(e :: SomeException) -> return ("",SockAddrUnix "")) --if nothing to sense from, get default value
  ms <- mapM tryReadMVar chans :: IO (M.Map Int (Maybe String))
  dt <- timediff timeRef cur
  oldCarState <- readIORef carRef
  oldDriveState <- readIORef driveRef
  monitorInfo <- monitorAction (oldCarState,oldDriveState)
  let rawCarState = (fromByteString msg){communications = ms,extra=monitorInfo}
  let carState = rawCarState{lapTimes = countLaps (lapTimes oldCarState, lastLapTime rawCarState)}
  --TODO this should not need to write everytime - find a way to detect when we are restarting and only save the carState then 
  writeIORef carRef carState
  return (dt, Just $ return carState) --TODO do i need the Event wrapper?
