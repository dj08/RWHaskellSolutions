-- ++
mypp :: [a] -> [a] -> [a]
mypp [] ys = ys
mypp (x:xs) ys =
  case xs of
   [] -> x : ys
   _  -> x : mypp xs ys

-- concat
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

-- reverse
myReverse :: [a] -> [a]
myReverse [] = []
-- myReverse xs = (last xs) : myReverse (init xs) -- could be inefficient
myReverse (x:xs) = myReverse (xs) ++ [x] -- would be as (in)efficient...(?)

-- and/or
myAnd :: [Bool] -> Bool
myAnd [] = False
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- all/any
-- Default cases copied from func definition.
myAll :: (a -> Bool) -> [a] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool

myAll fn (x:xs) = (fn x) && (myAll fn xs)
myAll fn []     = True

myAny fn (x:xs) = (fn x) || (myAny fn xs)
myAny fn []     = False


