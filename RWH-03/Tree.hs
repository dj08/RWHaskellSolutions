-- file: ch03/Tree.hs
-- Recursive, parameterized data type
data Tree a = Node a (Tree a) (Tree a)
              | Empty -- New value constructor, made by us
                deriving (Show, Eq)

-- An instance of a Tree
simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- Answer to RWH Ex 2, Pg 60
-- data Maybe a = Just a | Nothing
data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
                   deriving (Show)
-- why not Just (MaybeTree a) ?

-- How to get this working???
nextSimpleTree = MaybeNode "maybe parent"
                 (Just (MaybeNode "maybe left child" Nothing Nothing))
                 (Just (MaybeNode "maybe right child" Nothing Nothing))
-- We need 'Just' because the nodes of MaybeTree are 'Maybe (MaybeTree
-- a)', not 'MaybeTree a'

-- Exercises Q9: Height of a Tree
sortList (x:xs)
  | xs == [] = [x]
  | x > head (sortList xs) = [x] ++ sortList xs
  | x < last (sortList xs) = sortList xs ++ [x]
  | otherwise = [head xs] ++ sortList (x:tail xs)

treeHeight Empty = 0
treeHeight (Node _ a b) = 1 + max (treeHeight a) (treeHeight b)
  -- | a == Empty && b == Empty = 1
  -- | otherwise = 1 + childTreeHeight
      -- where childTreeHeight = head sortedHeights
      --       sortedHeights = sortList [(treeHeight a), (treeHeight b)]
