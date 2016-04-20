-- file: ch03/LocalFunctions.hs
pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"
-- pluralise "kuku" [0,1,2,3]
-- ["no kukus","one kuku","2 kukus","3 kukus"]
