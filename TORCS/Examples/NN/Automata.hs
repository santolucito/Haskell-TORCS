{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module TORCS.Examples.NN.Automata where

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
import Debug.Trace

--a drag racer that goes staight
nnDriver2 :: GasModel -> BrakeModel -> Driver
nnDriver2 gm bm = proc e -> do
    CarState{..} <- arr getE -< e
    a <- arr (gas gm) -< (rpm,speedX)
    b <- arr (b bm) -< (rpm,speedX)
    rec
       c' <- iPre  0 -< c
       ctr' <- iPre  0 -< ctr
       ctr <- arr (+1) -< ctr'
       c <- arr printLast -< (a,b,c',ctr)
    returnA -< defaultDriveState {accel = a, brakes = b, gear = 1}

printLast (a,b,c,ctr) = 
  if ctr ==500 then traceMe c else ((a-b)+c)

gas :: GasModel -> (Double,Double) -> Double
gas cm (r,s) = (model cm) (r,s)

b :: BrakeModel -> (Double,Double) -> Double
b cm (r,s) = (model cm) (r,s)

getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i

type GasModel = StdModel
                     (Vector 2)
                     (Vector 1)
                     (Double, Double)
                     (Double)

type BrakeModel = StdModel
                     (Vector 2)
                     (Vector 1)
                     (Double, Double)
                     (Double)
