-- program that transposes text in a file
-- uses the IO framework from RWH
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = id

-- transpose a given String
stringTranspose :: String -> String
-- stringTranspose [] = []
stringTranspose xs = unlines (zipWith func (head xs) (last xs))
           where func a b = [a,b]
-- DO NOT USE... ALL CORRUPT
