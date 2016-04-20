-- take
mytake :: Int -> [a] -> [a]
mytake 0 xs     = []
mytake n []     = []
mytake n (x:xs) = x : mytake (n-1) xs

-- drop
myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop n []     = []
myDrop n (x:xs) = myDrop (n-1) xs -- just go beheading the list!

-- splitAt
mySplitAt :: Int -> [a] -> ([a], [a])
-- Input and output data types differ. So we cannot apply recursion here...
mySplitAt n xs = (take n xs, drop n xs)

-- takeWhile
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile fn [] = []
myTakeWhile fn (x:xs)
  | fn x == True  = x : myTakeWhile fn xs
  | fn x == False = myTakeWhile fn xs
  | otherwise     = []

-- dropWhile
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile fn [] = []
myDropWhile fn (x:xs)
  | fn x == True  = myDropWhile fn xs
  | fn x == False = x : myDropWhile fn xs
  | otherwise     = []

-- break, span - no big deal!
