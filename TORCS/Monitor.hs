module TORCS.Monitor where

import TORCS.Types

monitorWrapper :: (CarState,DriveState) -> IO String
monitorWrapper (cs,ds) = do
  return ""
