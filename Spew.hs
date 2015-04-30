{-# LANGUAGE ScopedTypeVariables #-}

module Spew where

import Data.Array (listArray, Array)
import qualified Data.Array as A
import System.Random
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Control.Applicative
import qualified Data.Vector as V
import Debug.Trace


type PrimFastModel = [(String, FrequencySelector)]
type FastModel = Array Int (String, FrequencySelector)

type FrequencySelector = V.Vector Int
type WeightedGenerator = State (StdGen, FrequencySelector) Int
type ModelWalker = State StdGen Int

toFrequencySelector :: [(Int, Int)] -> FrequencySelector
toFrequencySelector = V.fromList . concatMap stretch
    where stretch (fr, idx) = replicate fr idx

nextWeightedRandom :: WeightedGenerator
nextWeightedRandom = do
  (g, fs ) <- get
  let (i, g') = randomR (0, V.length fs - 1) g
  put (g', fs)
  return (fs V.! i)

genWeightedRandom :: StdGen -> FrequencySelector -> Int
genWeightedRandom = curry $ evalState nextWeightedRandom


deserialize :: String -> PrimFastModel
deserialize = map (makeTrans . read) .  lines
    where makeTrans (s, is) = (s, toFrequencySelector is)

fromPrim :: PrimFastModel -> FastModel
fromPrim xs = listArray (0, length xs) xs

walkModel :: FastModel -> Int -> WriterT [String] (State StdGen) Int
walkModel model idx = do
            let (s, fs) = model A.! idx
            tell [s]
            g <- get
            let (i, (g', _)) = runState nextWeightedRandom (g, fs)
            put g'
            return i




-- processFastModel :: StdGen -> PrimFastModel -> FastModel
-- processFastModel g pfs = listArray (1, length processed) processed
--     where processed = map createTransitions pfs
--           createTransitions (s, fs) = (s, weightedRandomList (g, fs))

-- transition :: FastModel -> Int -> [String]
