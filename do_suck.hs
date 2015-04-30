import Suck
import Network.HTTP

-- For simplicity, we just use redirects in the shell to get data in/out
-- so this should be called as ./do_suck < data/urls.txt > sokal.model
main :: IO ()
main = do
  urls <- getContents
  pageBodies <- mapM (\url -> simpleHTTP (getRequest url) >>=
                              getResponseBody) $ lines urls
  putStr $ unlines $ map show $ bodyTextsToModel pageBodies
