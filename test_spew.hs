import Spew
import System.Random
import Test.QuickCheck
import Control.Monad
import Control.Applicative
import Data.Maybe

import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M



countOccurrences :: (F.Foldable t, Ord a) => t a -> M.Map a Int
countOccurrences = F.foldl countingInsert M.empty
    where countingInsert = flip $ (flip $ M.insertWith (+)) 1


countOccurrences' :: (F.Foldable t, Ord a) => t a -> [(a, Int)]
countOccurrences' = M.assocs . countOccurrences

genPairs :: Gen [(Int, Int)]
genPairs = sized (return . ($ zip [1..] [1..]) . take)

genFreqSelector :: Gen FrequencySelector
genFreqSelector = toFrequencySelector <$> genPairs

genGenerator :: Gen StdGen
genGenerator = arbitrary >>= return . mkStdGen

genIncreasingRandomList :: Gen [Int]
genIncreasingRandomList = fmap weightedRandomList $
                          (,) <$> genGenerator <*> genFreqSelector


prop_CorrectLength :: [(Int, Int)] -> Bool
prop_CorrectLength ns = (V.length $ toFrequencySelector ns) == totalCount
    where totalCount = sum $ map fst ns

prop_CorrectOccurrences :: FrequencySelector -> Bool
prop_CorrectOccurrences = all (uncurry (==)) . countOccurrences'


prop_CorrectDistribution :: [Int] -> Bool
prop_CorrectDistribution xs = all diffOk $ zip xs $ drop 1 xs
    where diffOk (x, y) = abs (x - y) < epsilon
          epsilon = 10




main = do
  quickCheck $ forAll genPairs prop_CorrectLength
  quickCheck $ forAll genFreqSelector prop_CorrectOccurrences
  quickCheck $ forAll genIncreasingRandomList prop_CorrectDistribution
