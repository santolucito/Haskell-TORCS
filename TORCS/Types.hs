{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TORCS.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe 

import FRP.Yampa

import Debug.Trace
traceMe x = traceShow x x 

-- | A driver observes the environment and changes the drive state
type Driver = SF (Event CarState) DriveState

-- | The drive state dictates how we move around the world
--   see the TORCS manual for full description (https://arxiv.org/pdf/1304.1672)
data DriveState = DriveState {
   gear      :: Int
  ,clutch    :: Double
  ,focus     :: [Int]
  ,accel     :: Double
  ,meta      :: Int --0 or (1 restart race and end yampa loop)
  ,brakes    :: Double
  ,steer     :: Double
  ,broadcast :: Message
} deriving (Show)

type Message = String

defaultDriveState = DriveState {
  gear   = 1
 ,clutch = 0
 ,focus  = [-90, -45, 0, 45, 90]
 ,accel  = 1
 ,meta   = 0
 ,brakes = 0
 ,steer  = 0
 ,broadcast = ""}


-- | car state is everything we can observe in the world
--   see the TORCS manual for full description (https://arxiv.org/pdf/1304.1672)
data CarState = CarState {
   z              :: Double
  ,angle          :: Double
  ,gear'          :: Int
  ,trackPos       :: Double
  ,speedY         :: Double
  ,distRaced      :: Double
  ,speedZ         :: Double
  ,damage         :: Double
  ,wheelSpinVel   :: [Double] -- length 4
  ,focus          :: [Int]
  ,track          :: [Double]
  ,curLapTime     :: Double
  ,speedX         :: Double
  ,racePos        :: Int
  ,fuel           :: Double
  ,distFromStart  :: Double
  ,opponents      :: [Double] 
  ,rpm            :: Double
  ,lastLapTime    :: Double
  ,communications :: Communications
} deriving (Show)

type Communications = M.Map Int (Maybe String)

defaultCarState = CarState {
   z         = 0
  ,angle     = 0
  ,gear'     = 1
  ,trackPos  = 0
  ,speedY    = 0
  ,distRaced = 0
  ,speedZ    = 0
  ,damage    = 0
  ,wheelSpinVel = [0,0,0,0] -- length 4
  ,focus     = [0]
  ,track     = replicate 19 0
  ,curLapTime = 0
  ,speedX    = 0
  ,racePos   = 1
  ,fuel      = 100
  ,distFromStart = 0
  ,opponents = [0]
  ,rpm       = 0
  ,lastLapTime = 0
  ,communications = M.empty
}
