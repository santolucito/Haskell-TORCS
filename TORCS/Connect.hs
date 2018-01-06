module TORCS.Connect (startDriver_,startDriver,startGUIDriver,startDrivers) where

import qualified Data.Map as M
import Data.Tuple
import qualified Control.Monad.Parallel as P
import Control.Concurrent.MVar

import TORCS.Types
import TORCS.Parser
import TORCS.Connect.YampaRunner (startDriverWithPort)

-- | if starting a single driver, we dont need any communication channels (mvar)
startDriver :: Driver -> IO(CarState, DriveState)
startDriver d    = startDriverWithPort False M.empty d 0 "3001" 

-- | start a single driver but dont get the final state of the car at the end
startDriver_ :: Driver -> IO()
startDriver_ d   = startDriver d >> return ()

-- | Use this if you want to watch the car drive in TORCS 
--   requires booting up TORCS manually
startGUIDriver d = startDriverWithPort True M.empty d 0 "3001" 

-- | To simulate platooning
--   we create comm channels between all vehicles that we start
-- TODO must seperate startTORCS IO from startDriverWtihPort to have everyone race in same torcs instance
startDrivers  :: [Driver] -> IO()
startDrivers ds = do
  mvars' <- mapM (\x->newEmptyMVar) ds :: IO [MVar String]
  let 
    mvars = M.fromList $ map swap $ zip mvars' [3001..] :: M.Map Int (MVar String)
    ps = map show [3001..]
    ts = map (*1000000) [0,1..]
    cs = zip (zip ds ts) ps
  P.mapM_ ((uncurry. uncurry) (startDriverWithPort False mvars)) cs 
  return ()

