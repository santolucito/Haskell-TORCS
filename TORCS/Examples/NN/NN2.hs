{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}

module TORCS.Examples.NN.NN2 where

import FRP.Yampa hiding (derivative)

import TORCS.Types
import TORCS.ConnectVerbose
import Control.Concurrent
import TORCS.Examples.NN.Automata
import Debug.Trace

import System.Process

import           Control.Category
import           Control.Monad.Random
import           Data.MyPrelude
import           Data.Utils
import           Numeric.Neural
import           Prelude              hiding ((.))
import           Pipes.Core

import Data.IORef


learnDriver :: IO ()
learnDriver = do
    gm <- modelR gasModel 
    bm <- modelR brakeModel 
    cnt <- newIORef 0
    runEffect $
            geneticTrainL (\m-> startDriverNN' gm m) 10 (0,(gm,bm))
        >-> reportTSP 1 (report cnt)
        >-> consumeTSP check

  where

    brakeModel :: GasModel
    brakeModel = mkStdModel
        (logisticLayer . (logisticLayer :: Layer 2 4)) -- trying out 4 hidden nodes
        (\b -> Diff $ Identity . sqDiff (cons (fromDouble b) nil))--(cons (fromDouble up) (cons (fromDouble down) (cons (fromDouble same) nil))) ) --cost fxn for probs 
        (\(rpm,speed) -> cons rpm (cons (speed) nil)) --convert input signal to a vector of doubles
        (\v -> vhead v) --, vhead $ vtail $ vtail v))
    
    gasModel :: GasModel
    gasModel = mkStdModel
        (logisticLayer . (logisticLayer :: Layer 2 4)) -- trying out 4 hidden nodes
        (\(gas) -> Diff $ Identity . sqDiff (cons (fromDouble gas) nil))--(cons (fromDouble up) (cons (fromDouble down) (cons (fromDouble same) nil))) ) --cost fxn for probs 
        (\(rpm,speed) -> cons rpm (cons (speed) nil)) --convert input signal to a vector of doubles
        (\v -> vhead v) --, vhead $ vtail $ vtail v))
  

startDriverNN' :: GasModel -> BrakeModel -> IO Double
startDriverNN' gm bm = do
       rawOut <- startDriverVerbose $ automataRacer gm bm
       print $ ((\(x,y,z) -> z) .head) rawOut
       return $ ((\(x,y,z) -> z) .head) rawOut

report cnt ts = do
      putStrLn ""
      putStrLn ""
      modifyIORef cnt (+1)
      curr <- readIORef cnt
      putStrLn $ (show $ curr*50) ++ " tests"
      print "current best - "
      (startDriverNN' $ tsModel ts) >>= print
      putStrLn ""
      putStrLn ""

      
{-      putStrLn $ "error = "++(show $ tsBatchError ts)
      createProcess (shell "torcs" ) {std_out = CreatePipe}
      threadDelay 10000000
      putStrLn "starting practice"
      createProcess $ shell "./startPractice.sh"
      threadDelay 2000000
      _ <- startGUIDriverVerbose $ dragRacer $ tsModel ts
      threadDelay 4000000
      createProcess $ shell "./killTORCS.sh"
      threadDelay 4000000-}
      putStrLn ""

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
    if ct > timeout then 1 else 0
  where
    timeout = 10

instance Scalable (Double,Double) where
  scale (x,y) s = 
   let f w = max 0 $ min 1 $ s*w
   in (f x, f y)

instance Scalable (Double,Double,Double) where
  scale (x,y,z) s = 
   let f w = max 0 $ min 1 $ s*w
   in (f x, f y, f z)

