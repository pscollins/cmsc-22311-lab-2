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

-- We pay a space cost up front for this, but save ourselves
-- computation later down the line. Given that most state transitions
-- have a frequency of only 1 or 2 (based on a cursory look over
-- sokal.model), it seems likely that the space cost won't be high.
type FrequencySelector = V.Vector Int


toFrequencySelector :: [(Int, Int)] -> FrequencySelector
toFrequencySelector = V.fromList . concatMap stretch
    where stretch (fr, idx) = replicate fr idx


deserialize :: String -> PrimFastModel
deserialize = map (makeTrans . read) .  lines
    where makeTrans (s, is) = (s, toFrequencySelector is)

fromPrim :: PrimFastModel -> FastModel
fromPrim xs = listArray (1, length xs) xs

randomIdx :: FastModel -> StdGen -> (Int, StdGen)
randomIdx m g = randomR (A.bounds m) g

walkModel :: FastModel -> WriterT [String] (State (StdGen, Int)) ()
walkModel model =
    do
      (g, idx) <- get
      let (s, fs) = model A.! idx
      let stuck = V.null fs
      tell [s ++ if stuck then "." else ""]
      let (i, g') = randomR (0, V.length fs - 1) g
      let (j, g'') = randomIdx model g
      put (g'', if stuck then j else fs V.! i)


runModel :: StdGen -> FastModel -> [String]
runModel g model = (evalState $ execWriterT $ mapM (\_ -> walker) [1..]) (g', idx)
    where walker = walkModel model
          (idx, g') = randomIdx model g

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f [] = []
takeUntil f [x] = [x]
takeUntil f (x:y:xs)
    | f y = [x, y]
    | otherwise = x:(takeUntil f (y:xs))

isTerminator :: Char -> Bool
isTerminator '.' = True
isTerminator '?' = True
isTerminator '!' = True
isTerminator _ = False

sentenceFinished :: String -> Bool
sentenceFinished = any isTerminator

takeAtLeast :: Int -> [String] -> [String]
takeAtLeast n ss = body ++ terminator
    where (body, rest) = splitAt n ss
          terminator = takeUntil sentenceFinished rest

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"
