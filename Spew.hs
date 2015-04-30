{-# LANGUAGE ScopedTypeVariables #-}

module Spew where

import Data.Array (listArray, Array)
import System.Random
import Control.Monad.State.Lazy
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace


type PrimTransitionFunction = [(String, FrequencySelector)]

type TransitionFunction = Array Int (String, [Int])
type FrequencySelector = V.Vector Int
type WeightedGenerator = State (StdGen, FrequencySelector) Int

deserialize :: String -> PrimTransitionFunction
deserialize = map (makeTrans . read) .  lines
    where makeTrans (s, is) = (s, toFrequencySelector is)

processTransitionFunction :: StdGen -> PrimTransitionFunction -> TransitionFunction
processTransitionFunction g pfs = listArray (1, length processed) processed
    where processed = map createTransitions pfs
          createTransitions (s, fs) = (s, weightedRandomList (g, fs))

toFrequencySelector :: [(Int, Int)] -> FrequencySelector
toFrequencySelector = V.fromList . concatMap stretch
    where stretch (fr, idx) = replicate idx fr

nextWeightedRandom :: WeightedGenerator
nextWeightedRandom = do
  (g, fs ) <- get
  let (i, g') = randomR (0, V.length fs - 1) g
  put (g', fs)
  return (fs V.! i)

weightedRandomList :: (StdGen, FrequencySelector) -> [Int]
weightedRandomList = evalState $ mapM (\_ -> nextWeightedRandom) [1..]
