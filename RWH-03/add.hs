-- file: ch03/add.hs
-- Surprisingly, the brackets are required!
sumList (x:xs) = x + sumList xs
-- sumList x xs = x + sumList xs fails - needs 2 args
sumList [] = 0
