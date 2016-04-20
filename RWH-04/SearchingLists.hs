-- elem
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y    = True
  | otherwise = myElem x ys

-- notElem
myNotElem :: (Eq a) => a -> [a] -> Bool
myNotElem _ [] = True
myNotElem x (y:ys)
  | x == y    = False
  | otherwise = myNotElem x ys

-- isPrefixOf
myIsPrefixOf :: (Eq a) => [a] -> [a] -> Bool
myIsPrefixOf [] _  = True
myIsPrefixOf _  [] = False
myIsPrefixOf (x:xs) (y:ys) | x == y     = myIsPrefixOf xs ys

-- isInfixOf
myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf [] _  = True -- termination case 1
myIsInfixOf _  [] = False -- termination case 3
myIsInfixOf (x:xs) (y:ys) | x == y             = myIsInfixOf xs ys
                          | otherwise          = myIsInfixOf (x:xs) ys
                          -- | x /= y && ys == [] = False -- termination case 2
-- Point to notice - when we have exhaustive leaf level condition, we
-- often do not need to specify middle level values.

-- isSuffixOf
myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf [] _  = True
myIsSuffixOf _  [] = False
myIsSuffixOf (x:xs) (y:ys) | x == y && xs == ys = True
                           | otherwise          = myIsSuffixOf (x:xs) ys
-- A bug hides if we mention the first guard as x==y = xs==ys. Concise but fails for cases like [2,3] [1,2,2,3]

-- another smart implementation
isSuffixOf' _ [] = True
isSuffixOf' s xs = s == drop (length xs - length s) xs
