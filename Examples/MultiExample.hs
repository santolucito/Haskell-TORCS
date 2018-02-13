{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module TORCS.Examples.MultiExample where

import FRP.Yampa

import TORCS.Types
import TORCS.Connect
import TORCS.Parser

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace



-- | Start multiple drivers
--   each has a comm channel labelled starting at 3001
platoon = startDrivers [leader,follower]


-- | the leader recievers the follwers current speed 
--   and tells it to go faster or slower
leader :: Driver
leader = proc CarState{..} -> do
    g <- arr shifting -< (rpm,gear')
    s <- arr steering -< (angle,trackPos)
    a <- arr gas -< (speedX,s)
    followerSpeed <- arr getFollowerSpeed -< communications
    msg <- arr (\(myS,theirS)->trace ((show myS)++" "++(show theirS)) $ if myS>theirS then "go faster" else "go slower") -< (speedX,followerSpeed)
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}

getFollowerSpeed :: Communications -> Double
getFollowerSpeed comm = let
  followerMsg = M.lookup 3002 comm
  unwrap = fromMaybe "0". fromMaybe Nothing
 in
  readAsDouble $ B.pack $ unwrap $ followerMsg

-- | the follower controls thier own vehicle
--   but recieves extra speed instructions from the leader
follower :: Driver
follower = proc e -> do
    CarState{..} <- arr getE -< e
    g <- arr shifting -< (rpm,gear')
    s <- arr steering -< (angle,trackPos)
    a' <- arr gas -< (speedX,s)
    a <- arr adjustToRequest -< (a',communications)
    msg <- arr show -< speedX
    returnA -< defaultDriveState {accel = a, gear = g, steer = s, broadcast = msg}

-- check if anyone is asking me to go faster/slower
adjustToRequest :: (Double,Communications) -> Double
adjustToRequest (s,cs) = let 
  up   = fromIntegral $ fromEnum $ M.foldr (\c b-> b || c==Just "go faster") False cs
  down = fromIntegral $ fromEnum $ M.foldr (\c b-> b || c==Just "go slower") False cs
 in
  (up*0.8) -- (down*10)


-- Shared functions
targetSpeed = 60
  
gas :: (Double,Double) -> Double
gas (speed,steer) = 
  if speed < (targetSpeed-(steer*50)) then 0.5 else 0

shifting :: (Double,Int) -> Int
shifting (rpm,g) = if 
  | rpm > 7000 -> min 6 (g+1)
  | rpm < 3000 -> max 1 (g-1)
  | otherwise  -> g

steering :: (Double,Double) -> Double
steering (spd,trackPos) = let
  turns = spd*14 / pi
  centering = turns - (trackPos*0.1)
  clip x = max (-1) $ min x 1
 in
  clip centering
