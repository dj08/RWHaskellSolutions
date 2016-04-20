-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- This function takes anything and converts it into "List" type as above
-- Compiler figured out this definition itself, without any direction
-- fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList _      = Nil

-- Inverse of fromList
-- Answer to RWH Ex 1, Pg 60
toList :: List a -> [a]
toList (Cons x xs) = x : toList xs
toList _           = []

