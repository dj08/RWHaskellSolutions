-- file: ch03/Ex1.hs
-- Solution to exercises on Pg. 69-70
-- Q1, 2: Function to compute the length of a list
myListLength :: [a] -> Int
-- Approach: Recursively behead the list and keep a count
myListLength (x:xs) = 1 + myListLength xs
myListLength []     = 0
  
-- Q3: Calculate mean of a list
myListMean :: [Int] -> Float
myListMean xs = fromIntegral (myListSum xs) / fromIntegral (myListLength xs)
myListMean [] = 0

myListSum :: [Int] -> Int
myListSum (x:xs) = x + myListSum xs
myListSum [] = 0

myListMeanUsingWhere :: [Int] -> Float
myListMeanUsingWhere xs = listSumFloat xs / listLengthFloat xs
         where listSumFloat xs = fromIntegral (sumList xs)
               listLengthFloat xs = fromIntegral (myListLength xs)
               listLengthFloat [] = 1
               sumList (x:xs) = x + sumList xs
               sumList [] = 0

-- Q4: List to palindrome... awesome solution!
fromListToPalindrome :: [a] -> [a]
fromListToPalindrome (x:xs) = [x] ++ (fromListToPalindrome xs) ++ [x]
fromListToPalindrome [] = []

-- Q5: Determine if input is palindrome
-- reverseList :: [a] -> [a]
reverseList (x:xs) = (reverseList xs) ++ [x]
reverseList [] = []

-- isPalindrome :: [a] -> Bool
isPalindrome xs 
  | xs == reverseList xs = True
  | otherwise = False                                        

-- Another good solution (from internet)
-- isPalindrome xs
--     | length xs == 0 = True
--     | otherwise = (head xs) == (last xs) 
--         && isPalindrome (take ((length xs) - 2) (tail xs))

-- Q6: Sort a list of lists by length of sublists
-- Try sorting a simple list first
-- sortList :: (Ord a, Num [a]) => [a] -> [a]
sortList (x:xs)
  | xs == [] = [x]
  | x > head (sortList xs) = [x] ++ sortList xs
  | x < last (sortList xs) = sortList xs ++ [x]
  | otherwise = [head xs] ++ sortList (x:tail xs)
-- sortList [a] = [a] -- put the empty list guard or place this before pattern
-- sortList _ = _

-- Now sort list of lists
-- sortLoL :: [[a]] -> [[a]]
sortLoL (x:xs)
  | xs == [] = [x]
  | length x > length (head (sortLoL xs)) = [x] ++ sortLoL xs
  | otherwise = [head xs] ++ sortLoL (x:tail xs)
-- Done! the tail condition is simply an optimization base case... hmm...

-- Q7. Function that joins lists with a separator
intersperse :: a -> [[a]] -> [a]
intersperse seperator (l:ls) = l ++ [seperator] ++ intersperse seperator ls
intersperse _ [] = []

-- Q8. Function that joins lists with a separator, no sep at the end
-- intersperse' :: a -> [[a]] -> [a]
intersperse' seperator (l:ls)
  | ls /= [] {- ls is empty -} = l ++ [seperator] ++ intersperse' seperator ls
  | otherwise = l

-- From internet
-- intersperse' sep [] = []
-- intersperse' sep (x:[]) = x
-- intersperse' sep (x:xs) = x ++ [sep] ++ intersperse' sep xs

-- Q9: Height of a tree. Solved in Tree.hs

-- Q10 - Q13: Solving the Graham's scan
-- Q10
data Direction = Lft | Rght | Strght
                 deriving (Show)

-- Q11: Function that calculates the turn made by data points a,b,c
-- and returns a Direction
{- Algo:
1. Figure out the equation of line from a,b : f(x,y) = y - mx - k
2. If f(xc,yc) > 0 -> Direction = Left, < 0 --> Direction = Right
Assume coordinates are specified as tuple (x,y), and provided to us as a list of tuples
-}
-- getDirection :: [(Float, Float)] -> Direction
-- This function returns the line function that passes through pts a,b
-- a,b are tuples
-- getLine :: [(Float, Float)] -> (Float, Float)
-- Return value is (slope, const)
calculateLine (a:b:_)
  | xdiff /= 0 = (((snd b) - (snd a)) / xdiff, ((snd a) * (fst b) - (snd b) * (fst a)) / xdiff)
  | otherwise = (-1,0)
      where xdiff = ((fst b) - (fst a))

-- Determine the value of line function for pt p3
determineLineFunctionValue fLine c = (snd c) - (fst fLine) * (fst c) - (snd fLine)

-- Match a list of tuples
-- matchTuples (p1:p2:_) = (fst p1) - (snd p2)
getDirection (p1:p2:p3:_)
  | determineLineFunctionValue (calculateLine [p1,p2]) p3 > 0 = Lft
  | determineLineFunctionValue (calculateLine [p1,p2]) p3 < 0 = Rght
  | otherwise           = Strght

--------------------------IMPORTANT---------------------------------
-- A shortcut could have been to compute the cross product directly,
-- and get going. Even here, we are effectively doing the same.
--------------------------------------------------------------------

-- Give equation and pt to this function: (m,k) in y-mx+k and (x,y)in
-- pt coordinates lineThroughAB eqn pt = (snd pt) - (fst eqn) * (fst
-- pt) + (snd eqn) m = ((snd b) - (snd a)) / ((fst b) - (fst a)) k =
-- ((snd a) * (fst b) - (snd b) * (fst a)) / ((fst b) - (fst a))

-- getDirection (a:b:c)
--   | lineThroughAB c < 0 = Lft
--   | lineThroughAB c > 0 = Rght
--   | otherwise           = Strght
--      where lineThroughAB c = y_pt - m * x_pt + k
--            y_pt = snd c
--            x_pt = fst c
--            m = ((snd b) - (snd a)) / ((fst b) - (fst a))
--            k = ((snd a) * (fst b) - (snd b) * (fst a)) / ((fst b) - (fst a)) 

-- Q12: Compute directions for a list of points
computeDirections :: [(Float, Float)] -> [Direction]
computeDirections [] = error "At least 3 elements are required"
computeDirections (p1:pts)
--  | null p1 = [ Strght ] -- error "At least three points are required" -- Note that we cannot put a condition like this... because the pattern won't match at all.
  | length pts < 2              = error "At least three points are required"
  | length pts == 2             = [ getDirection [p1, pts !! 0, pts !! 1] ]
  | otherwise                   = [ (getDirection [p1, pts !! 0, pts !! 1]) ] ++ computeDirections pts
-- computeDirections (p1:p2:p3:pts)
-- --  | p2 == []) || (p3 == [])    = [ Strght ] -- error "At least three points are required"
--   | p3 == []                    = [ Strght ] -- error "At least three points are required"
--   | pts == []                   = [ getDirection [p1,p2,p3] ]
--   | otherwise                   = [ (getDirection [p1,p2,p3]), (getDirection [p2,p3,(head pts)]) ]

-- Q13: Implement Graham's scan for a set of points. The scan takes as
-- inputs a set of coordinates, and returns as output the Coordinates
-- that constitute a convex hull
-- grahamScan :: [(Float, Float)] -> [(Float, Float)]

-- grahamScan pts
--   | 

-- Function to sort given coordinates in ascending order: first by y, then by x
data LTorNot = Smaller | NotSmaller
              deriving (Show, Eq)

compareCoordinates :: [(Float, Float)] -> LTorNot
compareCoordinates (p1:p2:_)
  | (snd p1) < (snd p2)                           = Smaller  -- p1 is less than p2
  | (snd p1) > (snd p2)                           = NotSmaller  -- p1 is not less than p2
  | (snd p1) == (snd p2) && (fst p1) < (fst p2)   = Smaller
  | otherwise                                     = NotSmaller

-- sortCoordinates :: [(Float, Float)] -> [(Float, Float)]
sortCoordinates [x] = [x]
sortCoordinates (p1:pts)
  | compareCoordinates [p1, head (sortCoordinates pts)] == Smaller     = [p1] ++ sortCoordinates pts -- All set
  | compareCoordinates [p1, head (sortCoordinates pts)] == NotSmaller  = [head (sortCoordinates pts)] ++ [p1] ++ tail (sortCoordinates pts)
--  | otherwise                                                          = [p1] ++ pts
