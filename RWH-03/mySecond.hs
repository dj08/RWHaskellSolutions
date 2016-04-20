-- file: ch03/mySecond.hs
-- return the second element of a simple list
mySecond :: [a] -> a

mySecond xs = if null (tail xs)
                 then error "list too short"
              else head (tail xs)

-- Using Maybe safety belt
safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing -- Definition for null input condition
safeSecond xs = if null (tail xs)
                   then Nothing -- Definition for too short input condition
                else Just (head (tail xs))

-- More readable version
tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
