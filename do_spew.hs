import Spew
import System.Random
import System.IO
import System.Environment (getArgs)

parseArgs :: [String] -> Int
parseArgs [] = 100
parseArgs [x] = read x


main = do
  gen <- getStdGen
  argv <- getArgs
  input <- readFile "sokal.model"
  let model = fromPrim $ deserialize $ input
  let (startIdx, gen') = randomIdx model gen
  let text = dropWhile (not . sentenceFinished) $ runModel gen' model
  putStrLn $ linefill 72 $ takeAtLeast (parseArgs argv) text
