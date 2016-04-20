-- print first word of each line of its input
import System.Environment

-- main input = firstWordOfEachLine input
firstWordOfEachLine :: String -> String
firstWordOfEachLine = unlines . map (unwords . (take 1) . words) . lines

-- file: ch04/FirstWord.hs
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
input <- readFile inputFile
writeFile outputFile (function input)

main = mainWith myFunction
where mainWith function = do
args <- getArgs
case args of
[input, output] -> interactWith function input output
_ -> putStrLn "error: exactly two arguments needed"

-- replace "id" with the name of out function below
-- myFunction = id
myFunction input = 
  let lns = map words (lines input)
  in unlines (firstWords lns)
  where firstWords [] = []
        firstWords (x:xs) = if null x
                            then firstWords xs
                            else head x : firstWords xs
