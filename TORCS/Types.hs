{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module TORCS.Types where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import FRP.Yampa
-- | A driver observes the environment and changes the drive state
type Driver = SF (Event CarState) DriveState

-- | The drive state dictates how we move around the world
data DriveState = DriveState {
   gear   :: Int
  ,clutch :: Double
  ,focus  :: [Int]
  ,accel  :: Double
  ,meta   :: Double
  ,break :: Double
  ,steer  :: Double
}

toByteString :: DriveState -> ByteString
toByteString DriveState{..} = pack $
  "(gear " ++(show gear)++")"++
  "(clutch "++(show clutch)++")"++
  "(focus "++(show focus)++")"++
  "(accel "++(show accel)++")"++
  "(meta " ++(show meta)++")"++
  "(break "++(show break)++")"++
  "(steer "++(show steer)++")"

defaultDriveState = DriveState {
  gear = 1
 ,clutch = 0
 ,focus = [-90, -45, 0, 45, 90]
 ,accel = 1
 ,meta = 0
 ,break = 1
 ,steer = 0}


-- | car state is everything we can observe in the world
data CarState = CarState {
   z   :: Double
  ,angle  :: Double
  ,gear  :: Int
  ,trackPos  :: Double
  ,speedY :: Double
  ,distRaced :: Double
  ,speedZ :: Double
  ,damage :: Double
  ,wheelSpinVel :: [Double] -- length 4
  ,focus :: [Int]
  ,track :: [Double]
  ,curLapTime :: Double
  ,speedX :: Double
  ,racePos :: Int
  ,fuel :: Double
  ,distFromStart :: Double
  ,opponents :: [Double] -- maybe distance from me?
  ,rpm   :: Double
  ,lastLapTime :: Double
}

fromByteString :: ByteString -> CarState 
fromByteString s =
  defaultCarState

defaultCarState = CarState {
   z   = 0
  ,angle  = 0
  ,gear  = 1
  ,trackPos = 0
  ,speedY = 0
  ,distRaced = 0
  ,speedZ = 0
  ,damage = 0
  ,wheelSpinVel = [0,0,0,0] -- length 4
  ,focus = [0]
  ,track = [0]
  ,curLapTime = 0
  ,speedX = 0
  ,racePos = 1
  ,fuel = 100
  ,distFromStart = 0
  ,opponents = [0]
  ,rpm   = 0
  ,lastLapTime = 0
}
