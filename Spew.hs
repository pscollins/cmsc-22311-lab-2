module Spew where
import Data.Array

type ModelState = (String,[(Int,Int)])
type FastModel = Array Int ModelState

deserialize :: String -> FastModel
deserialize = buildArr . map read .  lines
    where buildArr ss = listArray (1, length ss) ss
