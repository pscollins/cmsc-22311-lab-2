{-# LANGUAGE ScopedTypeVariables #-}

module Spew where

import Data.Array (listArray, Array)
import System.Random
import Control.Monad.State.Lazy
import Control.Applicative
import qualified Data.Vector.Unboxed as V

type ModelState = (String, [(Int, Int)])
type FastModel = Array Int ModelState

type FrequencySelector = V.Vector Int

type WeightedGenerator = State (StdGen, FrequencySelector) Int

deserialize :: String -> FastModel
deserialize = buildArr . map read .  lines
    where buildArr ss = listArray (1, length ss) ss


toFrequencySelector :: [(Int, Int)] -> FrequencySelector
toFrequencySelector = V.fromList . concatMap stretch
    where stretch (fr, idx) = replicate idx fr

nextWeightedRandom :: WeightedGenerator
nextWeightedRandom = do
  (g, fs ) <- get
  let (i, g') = randomR (0, V.length fs) g
  put (g', fs)
  return (fs V.! i)

-- nextRandom :: State ([(Int, Int)] Int
-- nextRandom = do
--   rnd <- get
--   let (i, rnd') = next rnd
--   put rnd'
--   return i


weightedRandomList :: (StdGen, FrequencySelector) -> [Int]
weightedRandomList = evalState $ repeat <$> nextWeightedRandom
