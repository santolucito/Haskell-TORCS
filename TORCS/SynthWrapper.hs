{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.SynthWrapper where

import FRP.Yampa
import TORCS.Types
import TORCS.Connect

import TORCS.Terms
--import TORCS.Simple
import TORCS.Complex
import TORCS.Complex2

import TORCS.Example hiding (steering)

runner :: Driver
runner = proc e -> do
  CarState{..} <- arr getE -< e
  (s,_) <- mainSF -< (angle, track, trackPos)
  (a,b,g) <- speedControl -<  (rpm,speedX,track, gear')
  returnA -< defaultDriveState {accel = a, gear = g, steer = s, brakes = b}
