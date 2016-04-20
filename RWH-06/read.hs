-- file: ch06/read.hs
main = do putStrLn "Give Double:"
          inpStr <- getLine
          let inpDouble = (read inpStr)::Double
          putStrLn ("Input is " ++ show inpDouble)
