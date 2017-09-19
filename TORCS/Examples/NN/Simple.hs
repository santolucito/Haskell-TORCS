{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module TORCS.Examples.NN.Simple where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect


import           Control.Category
import           Control.Monad.Random
import           Data.MyPrelude
import           Data.Utils
import           Numeric.Neural
import           Prelude              hiding ((.))
import           Pipes.Core

import System.Random
import System.IO.Unsafe

--a drag racer that goes staight
nnDriver :: CarModel -> Driver
nnDriver cm = proc e -> do
    CarState{..} <- arr getE -< e
    g <- arr (shifting cm) -< (rpm,gear')
    returnA -< defaultDriveState {accel = 1, gear = g}

shifting :: CarModel -> (Double,Int) -> Int
shifting cm (r,g) = let
  f = model cm
  (pUp, pDown, pSame) = traceMe $ (model cm) (r,g)
  rand = unsafePerformIO (randomIO :: IO Double)
 in if
  | rand < pUp -> min 6 (g+1)
  | rand > pUp && rand < pUp+pDown -> max 1 (g-1)
  | otherwise  -> g

getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i

type CarModel = StdModel
                     (Vector 2)
                     (Vector 3)
                     (Double, Int)
                     (Double, Double, Double)

