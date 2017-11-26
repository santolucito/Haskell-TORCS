{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module TORCS.Parser where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Maybe 

import TORCS.Types

import Debug.Trace
-- | the parser reads a s-expressions to a CarState
--   and writes a DriveState to a s-expression
--   TORCS sends all data as doubles, or lists of doubles, none nested

-- when packing DriveState to send to TORCS, do not include broadcast
-- broadcast is for internal MVars only
toByteString :: DriveState -> ByteString
toByteString DriveState{..} = B.pack $
  "(gear " ++(show gear)++")"++
  "(clutch "++(show clutch)++")"++
  "(focus "++(show focus)++")"++
  "(accel "++(show accel)++")"++
  "(meta " ++(show meta)++")"++
  "(brake "++(show brakes)++")"++
  "(steer "++(show steer)++")"

-- Again, do not include communications when decoding from server
-- communications is internal mvars only
fromByteString :: ByteString -> CarState 
fromByteString s = let
  fs' = B.splitWith (\c -> c==')' || c=='(') s
  fs = filter (/="") fs' :: [ByteString]
  ps = map (B.span (/=' ')) fs :: [(ByteString,ByteString)]
  fieldMap = M.fromList ps
  getField' s =  B.filter (/=' ') $ M.findWithDefault "" s fieldMap
  getField s = readAsDouble $ getField' s
  getList s = map readAsDouble $ tail $ B.split ' ' $ M.findWithDefault "" s fieldMap
 in
  --NB, restarting is handled by user
  if (s=="***restart***\NUL" || s=="" || s=="***shutdown***\NUL")
  then defaultCarState --TODO, make this Nothing?
  else defaultCarState 
     {angle = getField "angle", 
      speedX = getField "speedX", 
      speedZ = getField "speedZ", 
      rpm = getField "rpm", 
      distRaced = getField "distRaced", 
      lastLapTime = getField "lastLapTime", 
      curLapTime = getField "curLapTime", 
      gear' = floor $ getField "gear",
      fuel = getField "fuel", 
      trackPos = getField "trackPos",
      track = getList "track",
      damage = getField "damage"}

-- TODO someone has to have a better way of doing this
readAsDouble :: ByteString -> Double
readAsDouble s = let
  neg = B.head s == '-'
  s' = if neg then B.tail s else s
  (decPart, fracPart) = B.span (/='.') s'
  f = fromIntegral. fromMaybe 0. fmap fst. B.readInt
  frac = if B.length fracPart > 0 
    then (f $ B.tail fracPart) / (fromIntegral $ 10^(B.length $ B.tail fracPart))
    else 0
 in
  (if neg then -1 else 1) * ((f decPart) + frac)
  

