--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Synth.Accelerating where

import FRP.Yampa

import TORCS.Synth.Terms --provide implementation for names

--mainSF :: SF (speed , track) (accel , brakes)
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVals2 -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs


stateTrans :: ((Double , [Double]), (Double , Double), Int) -> ((Double , Double), Int)
stateTrans ((speed , track),(accel , brakes),state) = if
  | state==0 && ((not $ slow speed) && (clearahead track)) || ((slow speed) && (not $ clearahead track)) -> 
      (((speedup) , (offbrakes)), 0)
  | state==0 && ((slow speed) && (clearahead track)) || ((not $ slow speed) && (not $ clearahead track)) -> 
      (((deaccel) , (hitbrakes)), 0)


