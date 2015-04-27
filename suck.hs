module Suck where
import Data.Map

import Text.Html.TagSoup.Tree

type PrimitiveModel = Map (String, String) [String]
type ProcessedModel = [(String, [(Int, Int)])]


extractBody :: String -> [String]
extractBody s =
    let
        tags = parseTags s

in
