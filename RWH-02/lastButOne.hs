-- file: ch02/lastButOne.hs
-- lastButOne xs = if null $ last $ tail xs
--                    then head xs
--                    else lastButOne $ tail xs
-- My solution - uses only the very basic funcs taught in RWH/ch02
myLastButOne :: [a] -> a
myLastButOne xs = if null ( tail ( tail ( tail xs ) ) )
                   then head $ tail xs
                   else myLastButOne $ tail xs
-- From internet - proves better for 2 element lists.
internetLastButOne :: [a] -> a
internetLastButOne xs = if length xs == 2
                then head xs
                else internetLastButOne (tail xs)
