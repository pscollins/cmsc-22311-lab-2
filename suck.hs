module Suck where
import Data.Map hiding (foldl)
import Data.Monoid
-- import Data.Foldable

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Int, Int)])]


searchTree :: (TagTree String -> [TagTree String]) -> [TagTree String] -> [TagTree String]
searchTree f [] = []
searchTree f (b:ts) = f b ++ searchTree f ts

-- instance Foldable TagTree where
--     foldMap f


treeHasAttribute :: Attribute String -> TagTree String -> [TagTree String]
treeHasAttribute attr (TagLeaf _) = []
treeHasAttribute attr (TagBranch _  attrs children)
    | attr `elem` attrs = children
    | otherwise = []

extractBody :: String -> String
extractBody = innerText . flattenTree .
              searchTree (treeHasAttribute ("id", "body")) . tagTree . parseTags