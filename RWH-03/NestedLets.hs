-- file: ch03/NestedLets.hs
foo = let a = 1
      in let b = 1
         in a + b

myFoo = let a = 1
            b = 1
        in a + b

quux a = let a = "foo"
         in a ++ "eek!"

