{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module TORCS.Example where


import FRP.Yampa

import TORCS.Types
import TORCS.Connect

main :: IO ()
main = startDriver myDriver

myDriver :: SF (Event CarState) DriveState
myDriver = proc e -> do
    o <- arr sum_t -< e
    returnA -< o

sum_t :: Event CarState -> DriveState
sum_t e = case e of
  NoEvent -> defaultDriveState -- if no data, default
  Event i -> defaultDriveState --TODO calculate next move
