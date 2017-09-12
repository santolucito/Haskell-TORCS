{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Connect (startDriver_,startDriver,startGUIDriver,startDrivers) where

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

import FRP.Yampa 

import TORCS.Types
import TORCS.Parser


-- | if starting a single driver, we dont need any communication channels (mvar)
startDriver :: Driver -> IO(CarState, DriveState)
startDriver d    = startDriverWithPort False M.empty d 0 "3001" 

-- | start a single driver but dont get the final state of the car at the end
startDriver_ :: Driver -> IO()
startDriver_ d   = startDriver d >> return ()

-- | Use this if you want to watch the car drive in TORCS 
--   requires booting up TORCS manually
startGUIDriver d = startDriverWithPort True M.empty d 0 "3001" 

-- | To simulate platooning
--   we create comm channels between all vehicles that we start
startDrivers  :: [Driver] -> IO()
startDrivers ds = do
  mvars' <- mapM (\x->newEmptyMVar) ds :: IO [MVar String]
  let 
    mvars = M.fromList $ map swap $ zip mvars' [3001..] :: M.Map Int (MVar String)
    ps = map show [3001..]
    ts = map (*1000000) [0,1..]
    cs = zip (zip ds ts) ps
  P.mapM_ ((uncurry. uncurry) (startDriverWithPort False mvars)) cs 
  return ()

-- | have to wait for torcs to start before we try to connect
--   TODO use some shell stuff to get the status of torcs (or the port it opens)
--   spawnTORCS sock = do  
--     spawnProcess "torcs" ["-r /home/mark/.torcs/config/raceman/practice.xml"] --NB torcs requires full path
--     untilM_ (putStrLn "waiting on TORCS port..." >> threadDelay 10000) (isReadable sock)
--     putStrLn "TORCS port is open"


startDriverWithPort :: Bool -> M.Map Int (MVar String) -> Driver -> Int -> ServiceName -> IO (CarState,DriveState)
startDriverWithPort gui mvars myDriver delay port = withSocketsDo $ bracket connectMe close (yampaRunner myDriver mvars port)
  where
    connectMe = do
      unless gui $ spawnProcess "torcs" ["-r /home/mark/.torcs/config/raceman/practice.xml"] >> return ()--NB torcs requires full path
      threadDelay delay
      (serveraddr:_) <- getAddrInfo
                          (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                          Nothing (Just port)
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)

      -- send some mysterious message to get the race started
      let mysteryString = concat["SCR",(pack $ show port),"(init -90 -75 -60 -45 -30 -20 -15 -10 -5 0 5 10 15 20 30 45 60 75 90)"] 
      let attemptSend = sendTo sock mysteryString (addrAddress serveraddr)
      -- if the port was ready, then we can start recving msgs
      let tryRecv = timeout 10000 $ try (attemptSend >> recvFrom sock 1024) :: IO (Maybe (Either (SomeException) (ByteString, SockAddr)))
      iterateWhile ( isLeft. fromJust ) (putStr "." >> tryRecv )
--      _ <- either (\x -> print x >> print "port wasnt ready" >> threadDelay 100000 >> print "waited" >> sendTo sock mysteryString (addrAddress serveraddr) >>= print >> print "sent 2nd atmpt" >> recvFrom sock 1024) (return) x
      print "moving on"
      return sock

--the id (port number) is used to choose this car's writing mvar
-- this won't start until we are sure we are connected
yampaRunner :: Driver -> M.Map Int (MVar String) -> ServiceName -> Socket -> IO (CarState, DriveState)
yampaRunner myDriver allChannels id conn = do
  (msg,addr) <- recvFrom conn 1024
  t <- getCurrentTime
  timeRef <- newIORef t
  driveRef <- newIORef defaultDriveState
  carRef <- newIORef defaultCarState
  let myChannel = read id :: Int
  reactimate
    (return NoEvent)
    (sense timeRef conn allChannels carRef)
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
  --if we just sent a restart signal, end the reactimation (return True)
  --otherwise, continue (return False)
  if (meta msg == 1)
  then writeIORef outRef msg >> return True 
  else return False

mySwapMVar :: MVar a -> a -> IO Bool
mySwapMVar m v = do
  tryTakeMVar m
  tryPutMVar m v
  
-- sensing will try to read from all the mvars, and add this info to CarState
-- this only writes to the lapTimes part of CarState (not native to TORCS)
sense :: IORef UTCTime -> Socket -> M.Map Int (MVar String) -> IORef CarState -> Bool -> IO (DTime, Maybe (Event CarState))
sense timeRef conn chans carRef _ = do
  cur <- getCurrentTime
  (msg,d) <- catch (recvFrom conn 1024) (\(e :: SomeException) -> return ("",SockAddrUnix "")) --if nothing to sense from, get default value
  ms <- mapM tryReadMVar chans :: IO (M.Map Int (Maybe String))
  dt <- timediff timeRef cur
  let rawCarState = (fromByteString msg){communications = ms}
  oldCarState <- readIORef carRef
  let carState = rawCarState{lapTimes = countLaps (lapTimes oldCarState, lastLapTime rawCarState)}
  --TODO this should not need to write everytime - find a way to detect when we are restarting and only save the carState then 
  writeIORef carRef carState
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
