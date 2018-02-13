{-# LANGUAGE MultiWayIf #-}
module TORCS.Connect.Util where

import Data.IORef
import FRP.Yampa (DTime)
import Data.Time.Clock
import Control.Concurrent

-- | A small wrapper to keep track of how long each lap took
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

mySwapMVar :: MVar a -> a -> IO Bool
mySwapMVar m v = do
  tryTakeMVar m
  tryPutMVar m v
