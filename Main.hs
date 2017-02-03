{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Counter / Incrementer using events and with delay.
-- also uses pure haskell backend, which works on linux

-- sources:
--   https://wiki.haskell.org/Yampa/reactimate
--   incrementer

import Data.IORef
import Data.Time.Clock
import GHC.IO.Handle
import System.IO
import Data.Char
import Data.ByteString (ByteString)

import Control.Monad (unless)
import Network.Socket hiding (sendTo,recvFrom)
import Network.Socket.ByteString (sendTo,recvFrom)
import Control.Exception

import FRP.Yampa


port = "3001"
 
main :: IO ()
main = withSocketsDo $ bracket connectMe close handler
          where
            connectMe = do
              (serveraddr:_) <- getAddrInfo
                                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                  Nothing (Just port)
              sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
              connect sock (addrAddress serveraddr)
              sendTo sock "SCR(init -90 -75 -60 -45 -30 -20 -15 -10 -5 0 5 10 15 20 30 45 60 75 90)" (addrAddress serveraddr)
              return sock

handler :: Socket -> IO ()
handler conn = do
  (msg,d) <- recvFrom conn 1024
  hSetBuffering stdin NoBuffering
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate
    (return NoEvent)
    (sense timeRef conn)
    (action conn d)
    sigFun
 
action :: Socket -> SockAddr -> Bool -> String -> IO Bool
action conn d _ x = do
  let msg = "(gear 1.000)(clutch 0.000)(focus -90 -45 0 45 90)(accel 1.000)(meta 0.000)(brake 0.000)(steer 0.038)" :: ByteString
  print msg
  x <- sendTo conn msg d
  print x
  return False

sense :: IORef UTCTime -> Socket -> Bool -> IO (DTime, Maybe (Event String))
sense timeRef conn _ = do
  cur <- getCurrentTime
  (msg,d) <- recvFrom conn 1024
  dt <- timediff timeRef cur
  --print dt
  return (dt, Just $ return $ show msg)

timediff ::  IORef UTCTime -> UTCTime -> IO DTime
timediff ref cur = do
  old <- readIORef ref
  writeIORef ref cur
  return $ realToFrac $ diffUTCTime cur old

sigFun :: SF (Event String) String
sigFun = proc e -> do
    rec
      d <- iPre "" -< o
      o <- arr sum_t -< (e,d)
    returnA -< o

sum_t :: (Event String,String) -> String
sum_t (e,d) = case e of
  NoEvent -> d
  Event i -> d ++ i
