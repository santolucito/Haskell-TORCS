{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TORCS.Types where

import qualified Data.Map as M

import FRP.Yampa

-- | A driver observes the environment and changes the drive state
type Driver = SF CarState DriveState

-- | The DriveState dictates how the controller actuators behave and, by extension, how the car moves around the world.
--   See the TORCS manual for full description (<https://arxiv.org/pdf/1304.1672>)
data DriveState = DriveState {
   gear      :: Int
  ,clutch    :: Double
  ,focus     :: [Int]
  ,accel     :: Double
  -- | (0=continue) or (1=end simulation)
  ,meta      :: Int 
  ,brakes    :: Double
  ,steer     :: Double
  -- | non-native to torcs
  ,broadcast :: Message 
} deriving (Show)

-- | Broadcast messages are strings, but might be Text in the future
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


-- | CarState is everything the controller can observe in the world.
--   See the TORCS manual for full description (<https://arxiv.org/pdf/1304.1672>)
data CarState = CarState {
   z              :: Double
  ,angle          :: Double
  -- | /NB/: CarState uses prime version of identical DriveState field
  ,gear'          :: Int
  ,trackPos       :: Double
  ,speedY         :: Double
  ,distRaced      :: Double
  ,speedZ         :: Double
  ,damage         :: Double
  -- | length 4
  ,wheelSpinVel   :: [Double] 
  -- | /NB/: CarState uses prime version of identical DriveState field
  ,focus'         :: [Int]
  ,track          :: [Double]
  ,curLapTime     :: Double
  ,speedX         :: Double
  ,racePos        :: Int
  ,fuel           :: Double
  ,distFromStart  :: Double
  ,opponents      :: [Double] 
  ,rpm            :: Double
  ,lastLapTime    :: Double
  --nonnative to torrcs
  -- | non-native to TORCS
  ,lapTimes       :: [Double]
  -- | non-native to TORCS
  ,communications :: Communications 
  -- | non-native to TORCS
  ,monitor        :: String 
} deriving (Show)

-- | The global communications channel to read broadcasts from other vehicles.
--   The port for each vehicle is the index into the map (ports start at 3001 in TORCS)
type Communications = M.Map Int (Maybe Message)

defaultCarState = CarState {
   z         = 0
  ,angle     = 0
  ,speedX    = 0
  ,speedY    = 0
  ,speedZ    = 0
  ,rpm       = 0
  ,distRaced = 0
  ,lastLapTime = 0
  ,curLapTime = 0
  ,gear'     = 1
  ,fuel      = 100
  ,trackPos  = 0
  ,track     = replicate 19 0
  ,damage    = 0
  ,wheelSpinVel = [0,0,0,0] -- length 4
  ,focus'    = [0]
  ,racePos   = 1
  ,distFromStart = 0
  ,opponents = [0]
  ,lapTimes  = []
  ,communications = M.empty
  ,monitor = ""
}
