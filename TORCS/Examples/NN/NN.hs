{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}

module TORCS.Examples.NN.NN where

import FRP.Yampa hiding (derivative)

import TORCS.Types
import TORCS.ConnectVerbose
import Control.Concurrent
import TORCS.Examples.NN.Simple
import Debug.Trace

import System.Process

import           Control.Category
import           Control.Monad.Random
import           Data.MyPrelude
import           Data.Utils
import           Numeric.Neural
import           Prelude              hiding ((.))
import           Pipes.Core



learnDriver :: IO ()
learnDriver = do
    m <- modelR carModel 
    runEffect $
            (reinforcementBatchP startDriverNN 1000
        +>> reinforceDescentP m 1 (const 0.5))
        -- >-> reportTSP 50 report
        >-> consumeTSP check

  where

    -- the model takes the current rpm and current gear, and gives the probablity that we should shift up, down, or stay the same
    carModel :: CarModel
    carModel = mkStdModel
        (logisticLayer . (logisticLayer :: Layer 2 4)) -- trying out 4 hidden nodes
        (\(up,down,same) -> Diff $ Identity . sqDiff (cons (fromDouble up) (cons (fromDouble down) (cons (fromDouble same) nil))) ) --cost fxn for probs 
        (\(rpm,gear) -> cons rpm (cons (fromIntegral gear) nil)) --convert input signal to a vector of doubles
        (\v -> (vhead v,vhead $ vtail v, vhead $ vtail $ vtail v))
  
    -- run TORCS and collect input/output actions pairs with their cost
    -- TODO should use a dicounted delayed future reward of rather than whole race cost
    startDriverNN :: CarModel -> IO [((Double, Int), (Double, Double, Double), Double)] 
    startDriverNN cm = do
       rawOut <- startGUIDriverVerbose $ dragRacer cm
       let outD = map (\(cs,dr,c) -> ((rpm cs, gear' cs), (1,0,0), c)) rawOut --TODO need to get actions from gear change - will require looking at previous step (actually just folding here should work)
       print $ (\(cs,dr,c) -> c) $ head outD
       return outD

    --report = undefined
    check ts = return Nothing

----------
--
-- Car specific stuff
--
-----------

-- | What was the 'cost' of this driver based on its final states
calcCost :: (CarState,DriveState) -> Double
calcCost (c, d) =
  (damage c) + (curLapTime c)

dragRacer:: CarModel -> Driver
dragRacer cm = proc e -> do
  CarState{..} <- arr getE -< e
  driveState <- nnDriver cm -< e
  m <- arr restarting -< (distRaced,curLapTime)
  returnA -< driveState {meta = m}

-- restart after 250m or time out
restarting :: (Double,Double) -> Int
restarting (dist,ct) =
    if (dist>= 250) || ct > timeout then 1 else 0
  where
    timeout = 100

instance Scalable (Double,Double,Double) where
  scale (x,y,z) s = (s*x,s*y,s*z)

