--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module TORCS.Simple where

import FRP.Yampa
import TORCS.Types
import TORCS.Connect

import TORCS.Example hiding (steering)

import TORCS.Terms

mainSF :: SF (Double, Double, Double, Double, Double)  (Double, Int, Double)
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVals -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs

stateTrans :: ((Double, Double, Double, Double, Double), (Double, Int, Double),Int) -> ((Double,Int,Double),Int)
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
