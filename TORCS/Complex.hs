--TAMM converted to FRP.Yampa program
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module TORCS.Complex where

import FRP.Yampa

import TORCS.Terms --provide implementation for names

--mainSF :: SF (angle , track , trackpos) (steer , x)
mainSF = proc inSigs -> do
  rec
    outSigs' <- iPre initVals -< outSigs
    s' <- iPre 0 -< s 
    (outSigs,s)  <- arr stateTrans -< (inSigs,outSigs',s')
  returnA -< outSigs


stateTrans :: ((Double, [Double] , Double), (Double , Double), Int) -> ((Double , Double), Int)
stateTrans ((angle , track , trackpos),(steer , x),state) = if
  | state==0 && (((straight angle) && (not $ closetobend track)) && (clearroad track)) && (not $ atside trackpos) -> 
      (((turnmod track) ,  x), 0)
  | state==0 && ((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) || ((((not $ straight angle) && (not $ closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (((straighten angle) ,  x), 0)
  | state==0 && ((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) -> 
      (((steerside track) ,  x), 0)
  | state==0 && (((((not $ straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos))) || ((((straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (( steer ,  x), 0)

  | state==0 && ((((((not $ straight angle) && (closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos)) || ((((straight angle) && (closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos))) || ((((straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos))) || ((((not $ straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos)) -> 
      (((steerside track) , (turning)), 1)

  | state==0 && (((straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos) -> 
      (((turnmod track) , (turning)), 2)

  | state==0 && (((straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (atside trackpos) -> 
      (( steer , (turning)), 3)

  | state==0 && ((((not $ straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos)) || ((((not $ straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (atside trackpos)) -> 
      (((straighten angle) , (turning)), 4)

  | state==1 && (((straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos) -> 
      (((straighten angle) ,  x), 0)

  | state==1 && ((((((((((straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos)) && ((((straight angle) || (closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((straight angle) || (not $ closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((straight angle) || (closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos)) -> 
      (((steerside track) , (turning)), 1)

  | state==1 && (((straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos) -> 
      (((turnmod track) , (turning)), 2)

  | state==1 && ((((straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (atside trackpos)) -> 
      (( steer , (turning)), 3)

  | state==1 && ((((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((not $ straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos))) || ((((not $ straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos))) || ((((not $ straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (atside trackpos)) -> 
      (((straighten angle) , (turning)), 4)

  | state==2 && (((straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos) -> 
      (((turnmod track) ,  x), 0)
  | state==2 && (((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((not $ straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos))) || ((((straight angle) && (not $ closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (((straighten angle) ,  x), 0)
  | state==2 && ((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) || ((((straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (((steerside track) ,  x), 0)
  | state==2 && ((((straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((not $ straight angle) && (not $ closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (( steer ,  x), 0)

  | state==2 && ((((((((((straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos)) && ((((straight angle) || (closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((straight angle) || (closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (atside trackpos)) -> 
      (((turnmod track) , (turning)), 2)

  | state==3 && (((straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos) -> 
      (((turnmod track) ,  x), 0)
  | state==3 && ((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((not $ straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (((straighten angle) ,  x), 0)
  | state==3 && (((straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos) -> 
      (((steerside track) ,  x), 0)

  | state==3 && ((((((not $ straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos)) || ((((straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos))) || ((((not $ straight angle) && (closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos))) || ((((straight angle) && (closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos)) -> 
      (((turnmod track) , (turning)), 2)

  | state==3 && ((((((((((straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos)) && ((((straight angle) || (not $ closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((straight angle) || (not $ closetobend track)) || (clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (clearroad track)) || (atside trackpos)) -> 
      (( steer , (turning)), 3)

  | state==4 && ((((straight angle) && (closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((straight angle) && (closetobend track)) && (clearroad track)) && (not $ atside trackpos)) -> 
      (((straighten angle) ,  x), 0)

  | state==4 && ((((straight angle) && (closetobend track)) && (not $ clearroad track)) && (atside trackpos)) || ((((straight angle) && (closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos)) -> 
      (((turnmod track) , (turning)), 2)

  | state==4 && ((((((straight angle) && (not $ closetobend track)) && (clearroad track)) && (atside trackpos)) || ((((straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (atside trackpos))) || ((((straight angle) && (not $ closetobend track)) && (clearroad track)) && (not $ atside trackpos))) || ((((straight angle) && (not $ closetobend track)) && (not $ clearroad track)) && (not $ atside trackpos)) -> 
      (( steer , (turning)), 3)

  | state==4 && ((((((((((not $ straight angle) || (closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos)) && ((((not $ straight angle) || (closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (not $ atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (not $ clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (not $ closetobend track)) || (clearroad track)) || (atside trackpos))) && ((((not $ straight angle) || (closetobend track)) || (clearroad track)) || (atside trackpos)) -> 
      (((straighten angle) , (turning)), 4)


