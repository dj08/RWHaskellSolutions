-- zip
zip' :: [a] -> [b] -> [(a,b)]
-- zip' [] _  = []
-- zip' _  [] = []
-- zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

zip' xs ys | null xs || null ys = []
           | otherwise   = [((head xs), (head ys))] ++ zip' (tail xs) (tail ys)

-- zipWith
-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' fn xs ys | null xs || null ys  = []
                  | otherwise = [(fn (head xs) (head ys))] : zipWith' fn (tail xs) (tail ys)
-- Although you can't write the generic zipWithN function, you can still use foldr to perform a zipWithN style action on an arbitrary number of lists by wrapping your lists to zip as another list.

-- Prelude> foldr1 (zipWith (+)) [[1,2,3],[2,3,4],[3,4,5]]
-- [6,9,12]

