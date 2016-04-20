-- file: ch03/Nullable.hs
-- doesn't compile - complains of ambiguity
data Maybe a = Just a
               | Nothing
                 deriving( Show )

-- someBool = Just True

-- someString = Just "something"

