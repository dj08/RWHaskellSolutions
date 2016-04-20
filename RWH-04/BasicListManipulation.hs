-- Exercise to implement basic list manipulation functions

-- length
-- myLength :: [a] -> Int
myLength xs
  | xs == []    = 0
  | otherwise   = 1 + myLength (tail xs)

-- Hmm, clearer and does not require complications in type signature...
myLength' :: [a] -> Int
myLength' xs =
  case xs of
   [] -> 0
   _  -> 1 + myLength' (tail xs)

-- null
myNull :: [a] -> Bool
myNull [] = True
myNull _  = False -- Probably not a good practice... can misguide!

myNull' :: [a] -> Bool
myNull' xs =
  case xs of
   [] -> True
   _  -> False

-- head
myHead :: [a] -> a
myHead [] = error "list too short"
myHead (x:xs) = x

-- tail
myTail :: [a] -> [a]
myTail [] = error "list too short"
myTail (x:xs) = xs

-- last
myLast :: [a] -> a
myLast [] = error "list too short"
myLast (x:xs) = 
  case xs of 
   []   -> x
   _    -> myLast xs

-- init
myInit :: [a] -> [a]
myInit [] = error "list too short"
myInit (x:xs) =
  case xs of
   []   -> []
   _    -> x : myInit xs


