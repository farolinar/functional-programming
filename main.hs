import Data.List

sumList [] = 0
sumList (x:xs) = x + sumList xs

perms []  = [[]]
perms ls  = [ x:sisa | x<-ls, sisa <- perms (ls \\ [x])]

quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs,
                                  y <= x]
                   ++ [x] ++
                   quickSort [y | y <- xs,
                                  y > x]

main.hs:6:1: error:
    • Occurs check: cannot construct the infinite type: a ~ [a]
      Expected type: a -> [a]
        Actual type: [a] -> [[a]]
    • Relevant bindings include
        perms :: a -> [a] (bound at main.hs:6:1)
  |
6 | perms []  = [[]]


