--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Synth.SteeringSimple where

import FRP.Yampa

import TORCS.Synth.Terms --provide implementation for names

import Debug.Trace
--mainSF :: SF (angle , trackpos) steer
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVal -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs


stateTrans :: ((Double , Double), Double, Int) -> (Double, Int)
stateTrans ((angle , trackpos),steer,state) = if
  | state==0 && ((((((((not $ turningright angle) && (not $ turningleft angle)) && (rightofcenter trackpos)) && (leftofcenter trackpos)) || ((((not $ turningright angle) && (turningleft angle)) && (not $ rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((not $ turningright angle) && (not $ turningleft angle)) && (not $ rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((turningright angle) && (not $ turningleft angle)) && (not $ rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((turningright angle) && (turningleft angle)) && (rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((turningright angle) && (not $ turningleft angle)) && (not $ rightofcenter trackpos)) && (not $ leftofcenter trackpos)) -> 
      trace "going right" ((steerright angle), 0)
  | state==0 && ((((((not $ turningright angle) && (turningleft angle)) && (rightofcenter trackpos)) && (not $ leftofcenter trackpos)) || ((((not $ turningright angle) && (not $ turningleft angle)) && (rightofcenter trackpos)) && (not $ leftofcenter trackpos))) || ((((turningright angle) && (not $ turningleft angle)) && (rightofcenter trackpos)) && (not $ leftofcenter trackpos))) || ((((not $ turningright angle) && (turningleft angle)) && (not $ rightofcenter trackpos)) && (not $ leftofcenter trackpos)) -> 
      trace "going left" ((steerleft angle), 0)
  | state==0 && ((((((((not $ turningright angle) && (turningleft angle)) && (rightofcenter trackpos)) && (leftofcenter trackpos)) || ((((turningright angle) && (not $ turningleft angle)) && (rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((turningright angle) && (turningleft angle)) && (not $ rightofcenter trackpos)) && (leftofcenter trackpos))) || ((((turningright angle) && (turningleft angle)) && (rightofcenter trackpos)) && (not $ leftofcenter trackpos))) || ((((turningright angle) && (turningleft angle)) && (not $ rightofcenter trackpos)) && (not $ leftofcenter trackpos))) || ((((not $ turningright angle) && (not $ turningleft angle)) && (not $ rightofcenter trackpos)) && (not $ leftofcenter trackpos)) -> 
      trace "!!" ( 0, 0)


