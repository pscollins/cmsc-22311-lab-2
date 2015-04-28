module Suck where
import qualified Data.Map as M
import Data.Map (empty, insertWith, Map, mapWithKey, lookupIndex, unionsWith)
import Data.Tuple (swap)
import Data.Maybe
import Data.Monoid

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

type PrimitiveModel = Map (String, String) [String]
type FrequencyModel = Map (String, String) [(Int, String)]
type FrequencyCounter = Map String Int
type PreProcessedModel = Map (String, String) [(Int, Int)]
type ProcessedModel = [(String, [(Int, Int)])]


searchTree :: (TagTree String -> [TagTree String]) -> [TagTree String] -> [TagTree String]
searchTree f [] = []
searchTree f (b:ts) = f b ++ searchTree f ts

treeHasAttribute :: Attribute String -> TagTree String -> [TagTree String]
treeHasAttribute attr (TagLeaf _) = []
treeHasAttribute attr (TagBranch _  attrs children)
    | attr `elem` attrs = children
    | otherwise = concat $ map (treeHasAttribute attr) children

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

toProcessedModel :: FrequencyModel -> ProcessedModel
toProcessedModel m = map stripFirst $ M.assocs $ mapWithKey relabel m
    where stripFirst ((_, y), vs) = (y, vs)
          relabel (_, y) vs = mapMaybe idxOfPairs vs
              where idxOfPairs (count, z) =
                        lookupIndex (y, z) m >>= return . (flip (,)) count

mergePrimModels :: [PrimitiveModel] -> PrimitiveModel
mergePrimModels = (unionsWith (++))

bodyTextsToModel :: [String] -> ProcessedModel
bodyTextsToModel = process . mergePrimModels . getPrimModels
    where process = toProcessedModel . toFreqModel
          getPrimModels = map (toPrimModel . extractBody)
