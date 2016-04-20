-- map function
-- easy to implement via recursion

-- square every element
square2 xs = map squareOne xs
  where squareOne x = x * x

-- toUpper
upperCase2 xs = map toUpper xs

