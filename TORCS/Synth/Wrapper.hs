{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Synth.Wrapper where

import FRP.Yampa
import TORCS.Types
import TORCS.Connect

import TORCS.Synth.Terms
import qualified TORCS.Synth.Accelerating as A
import qualified TORCS.Synth.Gearing as G
import qualified TORCS.Synth.SteeringAdvanced as AS
import qualified TORCS.Synth.SteeringSimple as SS

import TORCS.Example hiding (steering)

import TORCS.Complex2

runner :: Driver
runner = proc e -> do
  c@CarState{..} <- arr getE -< e
--  (a,b) <- A.mainSF -< (speedX, track)
--  (g) <- G.mainSF -< (a, b, rpm, speedX)
  (a,b,g) <- speedControl -<  (rpm,speedX,track, gear')
  (s) <- SS.mainSF -< (angle, trackPos)
  --(s,_) <- AS.mainSF -< (angle, track, trackPos)
  returnA -< defaultDriveState {accel = a, gear = g, steer = s, brakes = b}
