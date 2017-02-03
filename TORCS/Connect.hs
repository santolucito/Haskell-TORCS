{-# LANGUAGE OverloadedStrings #-}
module TORCS.Connect (startDriver) where

import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import Data.IORef
import Data.Time.Clock
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import FRP.Yampa 

import TORCS.Types

port = "3001" :: ServiceName
 
{-
--| We want to be able to simulate platooning

startDrivers  :: [(Socket -> IO())] -> IO()
startDrivers ds = 
  map (forkIO. startDriver) ds
-}

startDriver :: Driver -> IO ()
startDriver myDriver = withSocketsDo $ bracket connectMe close (yampaRunner myDriver)
          where
            connectMe = do
              (serveraddr:_) <- getAddrInfo
                                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                  Nothing (Just port)
              sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
              connect sock (addrAddress serveraddr)
              _ <- sendTo sock "SCR(init -90 -75 -60 -45 -30 -20 -15 -10 -5 0 5 10 15 20 30 45 60 75 90)" (addrAddress serveraddr)
              return sock

--TODO make all this socket stuff into a Monad 
--and expose lower level api to allow for use with non-Yampa libraries
yampaRunner :: Driver -> Socket -> IO ()
yampaRunner myDriver conn = do
  (msg,d) <- recvFrom conn 1024
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate
    (return NoEvent)
    (sense timeRef conn)
    (action conn d)
    myDriver

action :: Socket -> SockAddr -> Bool -> DriveState -> IO Bool
action conn d _ s = do
  _ <- sendTo conn (toByteString s) d
  return False

sense :: IORef UTCTime -> Socket -> Bool -> IO (DTime, Maybe (Event CarState))
sense timeRef conn _ = do
  cur <- getCurrentTime
  (msg,d) <- recvFrom conn 1024
  dt <- timediff timeRef cur
  --print dt
  return (dt, Just $ return $ fromByteString msg)

timediff ::  IORef UTCTime -> UTCTime -> IO DTime
timediff ref cur = do
  old <- readIORef ref
  writeIORef ref cur
  return $ realToFrac $ diffUTCTime cur old
