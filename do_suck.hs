import Suck
import Network.HTTP

main :: IO ()
main = do
  urls <- getContents
  pageBodies <- mapM (\url -> simpleHTTP (getRequest url) >>=
                              getResponseBody) $ lines urls
  putStr $ unlines $ map show $ bodyTextsToModel pageBodies
