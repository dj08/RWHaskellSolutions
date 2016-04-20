-- lines
lines' :: String -> [String]
-- lines' (x:xs) =
--   case x of
--    "\n" -> [[""], lines' xs]
--    _    -> 
-- Recursion difficult... types are different
-- lines' ["\n"] = ["",""]
-- nextString xs = dropWhile (/='\n') xs
-- lines' xs = [takeWhile (/='\n') xs] ++ lines' (drop (length (takeWhile (/='\n') xs)) xs)
-- lines' xs | dropWhile (/='\n') xs] ++ lines' (drop (length (takeWhile (/='\n') xs)) xs)
lines' [] = []
lines' xs = line : lines' (drop (1 + length line) xs)
    where line = takeWhile (/='\n') xs

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

-- words/unwords
words' :: String -> [String]
words' [] = []
words' xs = word : words' (dropWhile isWhiteSpace rest)
    where (word, rest) = break isWhiteSpace xs
          isWhiteSpace x = elem x "\t\r\n "

-- another solution
splitWith _ [] = []
splitWith fn xs = filter (not . null) (first : remains) -- thanks to laziness
  where (first,rest) = break fn xs 
        remains = case rest of 
          _:ys ->splitWith fn ys
          _ -> []

-- similar to above!

-- another explicit hard working solution
words' :: String -> [String]
words' "" = []
words' (x:[]) = [(x : [])]
words' (x:s:xs)
  | elem x [' ', '\n', '\t', '\r'] = words' (s : xs)
  | elem s [' ', '\n', '\t', '\r'] = (x : "") : words' xs
  | otherwise = ( x : s : head(words' xs)) : tail(words' xs)

