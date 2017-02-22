--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Simple where

import FRP.Yampa
import TORCS.Types
import TORCS.Connect

import TORCS.Example hiding (steering)

import Debug.Trace 

runner :: Driver
runner = proc e -> do
  CarState{..} <- arr getE -< e
  (a,g,s) <- mainSF -< (angle, rpm, speedX, 100, trackPos)
  returnA -< defaultDriveState {accel = a, gear = g, steer = s}

initVals = (1,1,0)
--mainSF :: SF (angle , rpm , speed , targetspeed , trackpos) (accel , gear , steer)
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVals -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs

--stateTrans :: ((angle , rpm , speed , targetspeed , trackpos), (accel , gear , steer), Int) -> ((accel , gear , steer), Int)
stateTrans ((angle , rpm , speed , targetspeed , trackpos),(accel , gear , steer),state) = if
  | state==0 && ((lt speed(minus targetspeed(multp(abs steer)(1888)))) && (not $ lt rpm(3000))) && (gt rpm(5500)) -> 
      (((0.8) , (min(6)(plus gear(1))) , (steering angle trackpos)), 0)
  | state==0 && ((lt speed(minus targetspeed(multp(abs steer)(1888)))) && (lt rpm(3000))) && (not $ gt rpm(5500)) -> 
      (((0.8) , (max(1)(minus gear(1))) , (steering angle trackpos)), 0)
  | state==0 && ((lt speed(minus targetspeed(multp(abs steer)(1888)))) && (not $ lt rpm(3000))) && (not $ gt rpm(5500)) -> 
      (((0.8) ,  gear , (steering angle trackpos)), 0)
  | state==0 && ((not $ lt speed(minus targetspeed(multp(abs steer)(1888)))) && (not $ lt rpm(3000))) && (gt rpm(5500)) -> 
      (((0) , (min(6)(plus gear(1))) , (steering angle trackpos)), 0)
  | state==0 && ((lt speed(minus targetspeed(multp(abs steer)(1888)))) && (lt rpm(3000))) && (gt rpm(5500)) -> 
      (((0) , (min(6)(plus gear(1))) ,  steer), 0)
  | state==0 && ((not $ lt speed(minus targetspeed(multp(abs steer)(1888)))) && (lt rpm(3000))) && (not $ gt rpm(5500)) -> 
      (((0) , (max(1)(minus gear(1))) , (steering angle trackpos)), 0)
  | state==0 && ((not $ lt speed(minus targetspeed(multp(abs steer)(1888)))) && (not $ lt rpm(3000))) && (not $ gt rpm(5500)) -> 
      (((0) ,  gear , (steering angle trackpos)), 0)
  | state==0 && ((not $ lt speed(minus targetspeed(multp(abs steer)(1888)))) && (lt rpm(3000))) && (gt rpm(5500)) -> 
      (( accel , (min(6)(plus gear(1))) , (steering angle trackpos)), 0)

steering :: Double -> Double -> Double
steering angle trackPos = let
  turns = angle*14 / pi
  centering = turns - ((trackPos)*0.2)
  clip x = max (-1) $ min x 1
 in
  clip centering

lt :: Ord a => a -> a -> Bool
lt = (<)
gt :: Ord a => a -> a -> Bool
gt = (>)
minus :: Num a => a -> a -> a
minus = (-)
plus :: Num a => a -> a -> a
plus = (+)
multp = (*)

