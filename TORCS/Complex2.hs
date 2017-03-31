--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Complex2 (speedControl) where

import FRP.Yampa

import TORCS.Terms --provide implementation for names

--mainSF :: SF (rpm , speed , track) (accel , brakes , gear)
speedControl = proc inSigs -> do
  rec
    outSigs' <- iPre initVals2 -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs


stateTrans :: ((Double , Double , [Double], Int), (Double , Double , Int), Int) -> ((Double, Double, Int), Int)
stateTrans ((rpm , speed , track, gear'),(accel , brakes , gear),state) = if
  | state==0 && (((not $ slow speed) && (clearahead track)) && (acceling accel brakes)) || (((slow speed) && (clearahead track)) && (acceling accel brakes)) -> 
      (((speedup) , (offbrakes) , (shiftbyrpm gear' rpm)), 0)
  | state==0 && ((not $ slow speed) && (not $ clearahead track)) && (acceling accel brakes) -> 
      (((deaccel) , (hitbrakes) , (shiftbyrpm gear' rpm)), 0)
  | state==0 && ((slow speed) && (not $ clearahead track)) && (acceling accel brakes) -> 
      (( accel , (offbrakes) , (shiftbyrpm gear' rpm)), 0)

  | state==0 && (((not $ slow speed) && (clearahead track)) && (not $ acceling accel brakes)) || (((slow speed) && (clearahead track)) && (not $ acceling accel brakes)) -> 
      (((speedup) , (offbrakes) ,  gear), 1)
  | state==0 && ((slow speed) && (not $ clearahead track)) && (not $ acceling accel brakes) -> 
      (((deaccel) , (offbrakes) ,  gear), 1)
  | state==0 && ((not $ slow speed) && (not $ clearahead track)) && (not $ acceling accel brakes) -> 
      (((deaccel) , (hitbrakes) ,  gear), 1)

  | state==1 && (((not $ slow speed) && (clearahead track)) && (acceling accel brakes)) || (((slow speed) && (clearahead track)) && (acceling accel brakes)) -> 
      (((speedup) , (offbrakes) , (shiftbyspeed gear speed)), 0)
  | state==1 && ((slow speed) && (not $ clearahead track)) && (acceling accel brakes) -> 
      (((speedup) ,  brakes , (shiftbyspeed gear speed)), 0)
  | state==1 && ((not $ slow speed) && (not $ clearahead track)) && (acceling accel brakes) -> 
      (((deaccel) , (hitbrakes) , (shiftbyspeed gear speed)), 0)

  | state==1 && (((not $ slow speed) && (clearahead track)) && (not $ acceling accel brakes)) || (((slow speed) && (clearahead track)) && (not $ acceling accel brakes)) -> 
      (((speedup) , (offbrakes) ,  gear), 1)
  | state==1 && ((slow speed) && (not $ clearahead track)) && (not $ acceling accel brakes) -> 
      (((speedup) ,  brakes ,  gear), 1)
  | state==1 && ((not $ slow speed) && (not $ clearahead track)) && (not $ acceling accel brakes) -> 
      (((deaccel) , (hitbrakes) ,  gear), 1)


