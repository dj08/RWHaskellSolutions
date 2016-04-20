import Data.Maybe

-- file: ch04/ch04.exercises.hs
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : (fromMaybe [] (safeInit xs)))

-- otherwise - use inner recursion!
-- msafeInit :: [a] -> Maybe [a]
-- msafeInit [] = Nothing
-- msafeInit list = let work (x:[]) = []
--                      work (x:xs) = [x] ++ work xs
--                 in Just (work list)

-- too smart!
safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

ksafeHead = safeListFunc head
ksafeTail = safeListFunc tail
ksafeLast = safeListFunc last
ksafeInit = safeListFunc init

-- splitWith : similar to words but with *any* predicate
splitWith :: (a -> Bool) -> [a] -> [a]
splitWith _ [] = []
splitWith condition xs = start ++ splitWith condition (dropWhile condition rest)
  where (start, rest) = break condition xs

-- good one
-- splitWith' p x = case dropWhile (not . p) x of
--                   [] -> []
--                   nonEmpty -> pref : splitWith p suf
--                  where (pref, suf) = span p nonEmpty

-- print first word of each line of its input
-- refer ch04.exercises.03.hs

-- Pg. 97-99 exercises
-- 1,2,3 : asInt_... etc. Not done yet.

-- concat using foldr
concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- takeWhile using recursion
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = [] -- needed because the below def will not pattern match []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

-- takeWhile using foldr
takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
  where step x acc | f x = x : acc
                   | otherwise = [] -- not acc, because we want it to
--  *stop* at first failure. Notice that we do not need to break the
-- loop - laziness breaks it automatically when it realizes that we
-- are only ignoring further parameters!

-- groupBy using folds
-- groupBy' :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f xs = foldr step [] xs
  where step x acc | acc == []             = [[x]] -- no need of x == [] case
                   | f x (head (head acc)) = [x : (head acc)] ++ tail acc
                   | otherwise             = [x] : acc
-- above is not quite the same as groupBy... somehow.
-- groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy' f l = reverse $ foldl step [] l
-- where step (xs@(x:_):ys) e | f x e = (xs ++ [e]) : ys
-- step acc e = [e] : acc
