import Spew
import System.Random
import Test.QuickCheck
import Test.HUnit
import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Array (listArray, Array)
import qualified Data.Array as A


import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M



-- Quickcheck Tests
nextWeightedRandom :: WeightedGenerator
nextWeightedRandom = do
  (g, fs ) <- get
  let (i, g') = randomR (0, V.length fs - 1) g
  put (g', fs)
  return (fs V.! i)

weightedRandomList :: (StdGen, FrequencySelector) -> [Int]
weightedRandomList = evalState $ mapM (\_ -> nextWeightedRandom) [1..]

countOccurrences :: (F.Foldable t, Ord a) => t a -> M.Map a Int
countOccurrences = F.foldl countingInsert M.empty
    where countingInsert = flip $ (flip $ M.insertWith (+)) 1


countOccurrences' :: (F.Foldable t, Ord a) => t a -> [(a, Int)]
countOccurrences' = M.assocs . countOccurrences

genPairs :: Gen [(Int, Int)]
genPairs = sized (return . ($ zip [1..] [1..]) . take . constrain)
    where constrain = (max 1) . (mod 100) . (max 1) . abs


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
          epsilon = 1000

-- HUnit Tests
-- testStringModel = unlines ["(\"Hello\",[(2,1),(1,2)])"
--                           ,"(\"World\",[(1,2),(2,1)]"]

testStringModel = unlines ["(\"Hello\",[(2,1),(1,2)])"
                          ,"(\"World\",[(1,2),(2,1)])"]
testPrimModel = [("Hello", V.fromList [1, 1, 2])
                ,("World", V.fromList [2, 1, 1])]
walkTestModel = walkModel $ fromPrim $ deserialize testStringModel
testDeserialize = testPrimModel ~=? deserialize testStringModel

-- Test Execution

main = do
  quickCheck $ forAll genPairs prop_CorrectLength
  quickCheck $ forAll genFreqSelector prop_CorrectOccurrences
  quickCheck $ forAll genIncreasingRandomList $ not .  null
  quickCheck $ forAll genIncreasingRandomList (prop_CorrectDistribution . take 10000)
  runTestTT $ TestList [testDeserialize]
