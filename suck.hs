module Suck where
import qualified Data.Map as M
import Data.Map (empty, insertWith, Map)
import Data.Tuple (swap)
import Data.Monoid
-- import Data.Foldable

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

type PrimitiveModel = Map (String, String) [String]
type FrequencyModel = Map (String, String) [(Int, String)]
type FrequencyCounter = Map String Int
type ProcessedModel = [(String, [(Int, Int)])]


searchTree :: (TagTree String -> [TagTree String]) -> [TagTree String] -> [TagTree String]
searchTree f [] = []
searchTree f (b:ts) = f b ++ searchTree f ts

treeHasAttribute :: Attribute String -> TagTree String -> [TagTree String]
treeHasAttribute attr (TagLeaf _) = []
treeHasAttribute attr (TagBranch _  attrs children)
    | attr `elem` attrs = children
    | otherwise = []

extractBody :: String -> String
extractBody = innerText . flattenTree .
              searchTree (treeHasAttribute ("id", "body")) . tagTree . parseTags

toPrimModel :: String -> PrimitiveModel
toPrimModel = foldl updateMap (empty :: PrimitiveModel) . byTriples . words
    where byTriples ss = zip3 ss (drop 1 ss) (drop 2 ss)
          updateMap m (x, y, z) = insertWith (++) (x, y) [z] m

toFreqModel :: PrimitiveModel -> FrequencyModel
toFreqModel = M.map $ map swap . M.assocs . countFreq
    where countingInsert = flip $ (flip $ insertWith (+)) 1
          countFreq = foldl countingInsert (empty :: FrequencyCounter)
