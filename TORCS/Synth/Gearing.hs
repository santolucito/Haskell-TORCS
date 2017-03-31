--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Synth.Gearing where

import FRP.Yampa

import TORCS.Synth.Terms --provide implementation for names

--mainSF :: SF (accel , brakes , rpm , speed) gear
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVal -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs


stateTrans :: ((Double , Double , Double , Double), Int, Int) -> (Int, Int)
stateTrans ((accel , brakes , rpm , speed),gear,state) = if
  | state==0 && (not ( acceling accel brakes)) -> 
      ( gear, 0)

  | state==0 && acceling accel brakes -> 
      ((shiftbyspeed gear speed), 1)

  | state==1 && (not ( acceling accel brakes)) -> 
      ( gear, 0)

  | state==1 && acceling accel brakes -> 
      ((shiftbyrpm gear rpm), 1)


