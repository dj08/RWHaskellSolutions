-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

-- myInfo is the new data identifier. Not a function.
-- Book looks more like a function now... wonder, though.
----- Indeed, it is! Refer RWH Pg 43. value constructor (Book) is a
----function that creates and returns the new datatype we specified it
----with!
myInfo = Book 2455 "Algebra of Programming" ["Richard Bird", "et al."]

-- String can be renamed
data BookReview = BookReview BookInfo CustomerID String

-- type keyword is like alias
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

-- type for making complex data types
type BookRecord = (BookInfo, BookReview)
