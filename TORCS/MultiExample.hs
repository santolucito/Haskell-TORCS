{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.MultiExample where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect

import qualified Data.Map as M
import Debug.Trace


platoon = startDrivers [myDriver,myDriver]

myDriver :: Driver
myDriver = proc e -> do
    CarState{..} <- arr getE -< e
    g <- arr shifting -< (rpm,gear')
    s <- arr steering -< (angle,trackPos)
    a' <- arr gas -< (speedX,s)
    a <- arr adjustToRequest -< (a',communications)
    msg <- arr (\g->if g>4 then "go faster" else "go slower") -< g
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}

adjustToRequest :: (Double,Communications) -> Double
adjustToRequest (s,cs) = let 
  up   = fromIntegral $ fromEnum $ M.foldr (\c b-> b || c==Just "go faster") False cs
  down = fromIntegral $ fromEnum $ M.foldr (\c b-> b || c==Just "go slower") False cs
 in
  s + (up*10) - (down*(-10))



targetSpeed = 210

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 5500 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g
 
steering :: (Double,Double) -> Double
steering (angle,trackPos) = let
  turns = angle*14 / pi
  centering = turns - ((trackPos)*0.2)
  clip x = max (-1) $ min x 1
 in
  clip centering
  
gas :: (Double,Double) -> Double
gas (speed,steer) = 
  if speed < (targetSpeed-((abs steer)*1800)) then 0.8 else 0
  
getE :: Event CarState -> CarState
getE  e = case e of
  NoEvent -> defaultCarState -- if no data, default
  Event i -> i
