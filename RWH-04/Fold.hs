-- file: ch04/Fold.hs
-- foldl
niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

-- Studying foldr
-- implement 'filter' lookalike
myFilter fn xs = foldr step [] xs
   where step x acc | fn x = x : acc
                    | otherwise = acc

-- try filter using foldl
myFilterl fn xs = foldl step [] xs
  where step acc x | fn x = acc ++ [x] -- folding from the left, so push in same order!
                   | otherwise = acc

-- A lovely intuitive explanation that is correct only for associative
-- operators. Consider

-- foldr (-) 0 [1,2,3] == 2
-- 1 - (2 - (3 - 0)) = 2

-- foldl (-) 0 [1,2,3] == 2
-- (((0 - 1) - 2) - 3) = -6

-- -- 1 : 2 : 3 : []
-- -- 1 - 2 - 3 - 0 == -4

-- (-) as the example might be a good way to showcase the right
-- associativity of foldr.

-- map
myMap :: (a -> b) -> [a] -> [b]

myMap f xs = foldr step [] xs
  where step x acc = (f x) : acc -- cool!

myMapl :: (a -> b) -> [a] -> [b]
myMapl f xs = foldl step [] xs
  where step acc x = acc ++ [f x]

-- identity function in foldr
-- follows the inution: replace : with f and [] with z/acc
myId = foldr (:) [] [1..5]

-- reverse using foldr
myReverse xs = foldr f [] xs
  where f x z = z ++ [x] -- feeling better!

-- reverse using foldl
myReverse' xs = foldl f [] xs
  where f z x = x : z

myId' xs = foldl f [] xs
  where f z x = z ++ [x]

-- foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a -- wow!

myFoldl f z xs = foldr step id xs z -- (foldr step id xs) z
  where step x g a = g (f a x) -- Boom! Blew off my mind...

-- myFoldl f z (x:xs)
-- == step x (foldr step id xs) z |=> g == foldr step id xs, a == z

-- myFoldl (+) 0 [1:2:3:[]]
-- == foldr step id [1:2:3:[]] 0 where step x g a = g ((+) a x)
-- == step 1 (foldr step id [2:3:[]]) 0 where step x g a = g ((+) a x)
-- == step 1 (step 2 (step 3 (foldr step id []))) 0 where step x g a = g ((+) a x)
-- == step 1 (step 2 (step 3 (step [] id))) 0 where step x g a = g ((+) a x)
-- == (step 2 (step 3 (step [] id))) ((+) 0 1)
-- == (step 3 (step [] id)) ((+) ((+) 0 1) 2)
-- == (step [] id) ((+) ((+) ((+) 0 1) 2) 3)
-- == id ((+) ((+) ((+) ((+) 0 1) 2) 3) [])
-- == ((+) ((+) ((+) ((+) 0 1) 2) 3) [])

-- foldr in terms of foldl
myFoldr f z xs = foldl step id xs z
  where step x g a = x (f g a)

-- ++
mypp :: [a] -> [a] -> [a]
mypp xs ys = foldr (:) ys xs

mypp' :: [a] -> [a] -> [a]
mypp' xs ys = foldl (++) ys xs
