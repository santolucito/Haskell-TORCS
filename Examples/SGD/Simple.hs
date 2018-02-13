{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module TORCS.Examples.Learning.Simple where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect
import Control.Concurrent

import Control.Lens

data Thetas = Thetas{
   _speed :: Double,
   _turning :: Double
  } deriving (Show)

makeLenses ''Thetas

myDriver :: Thetas -> Driver
myDriver Thetas{..} = proc CarState{..} -> do
    g <- arr shifting -< (rpm,gear')
    s <- arr (steering _turning) -< (angle,trackPos)
    a <- arr (gas _speed) -< (speedX,s)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s}

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 7000 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g

steering :: Double -> (Double,Double) -> Double
steering turning (spd,trackPos) = let
  turns = spd*turning / pi
  centering = turns - (trackPos*0.1)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: Double -> (Double,Double) -> Double
gas targetSpeed (speed,steer) = 
  if speed < (targetSpeed-(steer*50)) then 0.5 else 0
