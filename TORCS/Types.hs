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
} deriving (Show)

toByteString :: DriveState -> ByteString
toByteString DriveState{..} = B.pack $
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
  ,gear'  :: Int
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
} deriving (Show)

fromByteString :: ByteString -> CarState 
fromByteString s = let
  fs' = B.splitWith (\c -> c==')' || c=='(') s
  fs = filter (/="") fs' :: [ByteString]
  ps = map (B.span (/=' ')) fs :: [(ByteString,ByteString)]
  fieldMap = M.fromList ps
  getField s = readAsDouble $ B.filter (/=' ') $ M.findWithDefault "" s fieldMap
 in
  traceMe defaultCarState 
     {angle = getField "angle", 
      speedX = getField "speedX", 
      speedZ = getField "speedZ", 
      rpm = getField "rpm", 
      fuel = getField "fuel", 
      trackPos = getField "trackPos",
      damage = getField "damage"}

traceMe x = traceShow x x 
-- TODO some has to have a better way of doing this
readAsDouble :: ByteString -> Double
readAsDouble s = let
  neg = B.head s == '-'
  s' = if neg then B.tail s else s
  (decPart, fracPart) = traceMe $ B.span (/='.') s'
  f = fromIntegral. fromMaybe 0. fmap fst. B.readInt
  frac = if B.length fracPart > 0 
    then (f $ B.tail fracPart) / (fromIntegral $ 10^(B.length $ B.tail fracPart))
    else 0
 in
  (if neg then -1 else 1) * ((f decPart) + frac)
  

defaultCarState = CarState {
   z   = 0
  ,angle  = 0
  ,gear'  = 1
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
