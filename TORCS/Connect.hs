module TORCS.Connect (startDriver_,startDriver,startGUIDriver,startDrivers) where

import qualified Data.Map as M
import Data.Tuple
import qualified Control.Monad.Parallel as P
import Control.Concurrent.MVar

import TORCS.Types
import TORCS.Parser
import TORCS.Connect.Runner (startDriverWithPort)

-- | This is the will boot TORCS and run the simulation saved in the practice config.
--   this is probably saved in @~\/.torcs\/config\/raceman\/practice.xml@, but should be changed via the TORCS GUI.
--   If the config file is elsewhere, you will need to change this library and recompile Haskell-TORCS from source
startDriver :: Driver -> IO(CarState, DriveState)
-- if starting a single driver, we dont need any communication channels (mvar)
startDriver d    = startDriverWithPort False M.empty d 0 "3001" 

-- | start a single driver as in startDriver, but dont get the final state of the car at the end.
startDriver_ :: Driver -> IO()
startDriver_ d   = startDriver d >> return ()

-- | Use this if you want to watch the car drive in TORCS.
--   Requires booting up TORCS manually and starting a track to the screen where TORCS says:
--   @...Initializing Driver scr_server1...@
startGUIDriver d = startDriverWithPort True M.empty d 0 "3001" 

-- | This allows you to run simulate multiple vehicles on the same track.
--   This will automatically connect the broadcast in DriveState to the communications in CarState for all other cars.
--   Platooning simulation will implement controllers that utilize this comm channel between all vehicles.
--   Each car's channel is addressable with their cooresponding TORCS port (starting at 3001)
startDrivers  :: [Driver] -> IO()
startDrivers ds = do
-- TODO must seperate startTORCS IO from startDriverWtihPort to have everyone race in same torcs instance
  mvars' <- mapM (\x->newEmptyMVar) ds :: IO [MVar String]
  let 
    mvars = M.fromList $ map swap $ zip mvars' [3001..] :: M.Map Int (MVar String)
    ps = map show [3001..]
    ts = map (*1000000) [0,1..]
    cs = zip (zip ds ts) ps
  P.mapM_ ((uncurry. uncurry) (startDriverWithPort False mvars)) cs 
  return ()

