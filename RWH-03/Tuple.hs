-- file: ch03/Tuple.hs
-- Pattern matching against a tuple
-- returns third element of tuple
-- Best is, that this is generally applicable!
third (a,b,c) = c

complicated (True, a, x:xs, 5) = (a, xs)
