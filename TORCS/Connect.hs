{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TORCS.Connect (startDriver,startDrivers) where

import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import qualified Control.Monad.Parallel as P
import Control.Concurrent
import Control.Concurrent.MVar

import Prelude hiding (concat)
import Data.IORef
import qualified Data.Map as M
import Data.Time.Clock
import Data.ByteString (ByteString,concat)
import Data.ByteString.Char8 (pack)
import Data.Tuple
import Data.Maybe

import System.Exit 

import FRP.Yampa 

import TORCS.Types
import TORCS.Parser


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
  P.mapM ((uncurry. uncurry) (startDriverWithPort mvars)) cs 
  return ()

-- if starting a single driver, we dont need any communication channels (mvar)
startDriver d = startDriverWithPort M.empty d 0 "3001"

startDriverWithPort :: M.Map Int (MVar String) -> Driver -> Int -> ServiceName -> IO ()
startDriverWithPort mvars myDriver delay port = withSocketsDo $ bracket connectMe close (yampaRunner myDriver mvars port)
  where
    connectMe = do
      threadDelay delay
      (serveraddr:_) <- getAddrInfo
                          (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                          Nothing (Just port)
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)
      let mysteryString = concat["SCR",(pack $ show port),"(init -90 -75 -60 -45 -30 -20 -15 -10 -5 0 5 10 15 20 30 45 60 75 90)"] 
      _ <- sendTo sock mysteryString (addrAddress serveraddr)
      return sock

--the id (port number) is used to choose this car's writing mvar
yampaRunner :: Driver -> M.Map Int (MVar String) -> ServiceName -> Socket -> IO ()
yampaRunner myDriver allChannels id conn = do
  (msg,addr) <- recvFrom conn 1024
  t <- getCurrentTime
  timeRef <- newIORef t
  let myChannel = read id :: Int
  reactimate
    (return NoEvent)
    (sense timeRef conn allChannels)
    (action conn addr (M.lookup myChannel allChannels))
    myDriver

-- action will do two things separately
-- first, send the drive instructions
-- second, broadcast the message to the other threads
action :: Socket -> SockAddr -> Maybe (MVar String) -> Bool -> DriveState -> IO Bool
action conn d myBroadcastChan _ msg = do
  bytesSent <- sendTo conn (toByteString msg) d 
  oldVal <- maybe (return Nothing) tryReadMVar myBroadcastChan
  _ <- maybe (return False) (\x -> if oldVal == (Just $ broadcast msg) then return False else mySwapMVar x (broadcast msg)) myBroadcastChan
  --if we just sent a restart signal, end the reactimation (return True)
  --otherwise, continue (return False)
  return $ if meta msg == 1 then True else False

mySwapMVar :: MVar a -> a -> IO Bool
mySwapMVar m v = do
  tryTakeMVar m
  tryPutMVar m v
  
-- sensing will try to read from all the mvars, and add this info to CarState
-- this will not write anything
sense :: IORef UTCTime -> Socket -> M.Map Int (MVar String) -> Bool -> IO (DTime, Maybe (Event CarState))
sense timeRef conn chans _ = do
  cur <- getCurrentTime
  (msg,d) <- catch (recvFrom conn 1024) (\(e :: SomeException) -> return ("",SockAddrUnix "")) --if nothing to sense from, get default value
  ms <- mapM tryReadMVar chans :: IO (M.Map Int (Maybe String))
  dt <- timediff timeRef cur
  let x = (fromByteString msg){communications = ms}
  --print dt
  return (dt, Just $ return x) --TODO do i need the Event wrapper?

timediff ::  IORef UTCTime -> UTCTime -> IO DTime
timediff ref cur = do
  old <- readIORef ref
  writeIORef ref cur
  return $ realToFrac $ diffUTCTime cur old
