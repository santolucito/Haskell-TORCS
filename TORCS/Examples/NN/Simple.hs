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
    a <- arr (gas cm) -< (rpm,speedX)
    b <- arr (b cm) -< (rpm,speedX)
    returnA -< defaultDriveState {accel = a, brakes = b, gear = 1}

gas :: CarModel -> (Double,Double) -> Double
gas cm (r,s) = --fst $ (model cm) (r,s)
	let
	  f = model cm
	  rand = unsafePerformIO (randomIO :: IO Double)
	 in 
	   if rand > (fst $ f (r,s)) then 1 else 0

b :: CarModel -> (Double,Double) -> Double
b cm (r,s) = --snd $ (model cm) (r,s)
	let
	  f = model cm
	  rand = unsafePerformIO (randomIO :: IO Double)
	 in 
	   if rand > (snd $ f (r,s)) then 0.3 else 0

getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i

type CarModel = StdModel
                     (Vector 2)
                     (Vector 2)
                     (Double, Double)
                     (Double, Double)

