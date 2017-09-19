{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module TORCS.Examples.Learning.NN where

import           Control.Category
import           Control.Monad.Random
import           Data.MyPrelude
import           Data.Utils
import           Numeric.Neural
import           Prelude              hiding ((.))
import           Pipes.Core


instance Scalable (Double,Double,Double) where
  scale (x,y,z) s = (s*x,s*y,s*z)

main' :: IO ()
main' = do
    m <- modelR carModel 
    runEffect $
            (reinforcementBatchP startDriverNN 1000
        +>> reinforceDescentP m 1 (const 0.5))
        >-> reportTSP 50 report
        >-> consumeTSP check

  where

    -- the model takes the current speed and current gear, and gives the probablity that we should shift up, down, or stay the same
    carModel :: CarModel
    carModel = mkStdModel
        (tanhLayer . (tanhLayer :: Layer 2 8)) -- trying out 8 hidden nodes 
        (\(up,down,same) -> Diff $ Identity . sqDiff (cons (fromDouble up) (cons (fromDouble down) (cons (fromDouble same) nil))) ) --cost fxn for probs 
        (\(speed,gear) -> cons speed (cons (fromIntegral gear) nil)) --convert input signal to a vector of doubles
        (\v -> (vhead v,vhead $ vtail v, vhead $ vtail $ vtail v))
  
    -- BIG TODO
    -- need to be able to run TORCS and collect input/output actions, as well as cost
    -- also, should use a dicounted future reward of rather than whole race cost
    startDriverNN :: CarModel -> IO [((Double, Int), (Double, Double, Double), Double)] 
    startDriverNN = undefined

    report = undefined
    check = undefined

type CarModel = StdModel
                     (Vector 2)
                     (Vector 3)
		     (Double, Int)
		     (Double, Double, Double)

{- mkStdModel
        (tanhLayer . (tanhLayer :: Layer 1 4))
        (\x -> Diff $ Identity . sqDiff (pure $ fromDouble x))
        pure
        vhead

    getError ts =
        let m = tsModel ts
        in  maximum [abs (sin x - model m x) | x <- [0, 0.1 .. 2 * pi]]

    report ts = liftIO $ do
        ANSI.clearScreen
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
        ANSI.setCursorPosition 0 0
        printf "Generation %d\n" (tsGeneration ts)
        ANSI.setSGR [ANSI.Reset]
        graph (model (tsModel ts)) 0 (2 * pi) 20 50
    
    check ts = return $ if getError ts < 0.1 then Just () else Nothing
-}
