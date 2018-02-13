module TORCS.Monitor where

import TORCS.Types

-- | This IO action will run at every step of the simulation 
--   and post the output the 'monitor' field of the CarState.
--   This is to allow for use of Runtime Monitoring tools such as <https://dl.acm.org/citation.cfm?id=1069573>
--   For now, in order to change this, you need to change Haskell-TORCS itself and recompile the library
monitorWrapper :: (CarState,DriveState) -> IO String
monitorWrapper (cs,ds) = do
  return ""
