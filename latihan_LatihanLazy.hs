pythaTriple = [(x,y,z) | z <- [5..], y <- [z-1, z-2..0], x <- [y-1, y-2..0], x*x + y*y == z*z]
-- take 4 pythaTriple
-- returns [(3,4,5),(6,8,10),(5,12,13),(9,12,15)]

splits [] = [([],[])]

splits (x:xs) = ([],x:xs) : [ (x:ps,qs) | (ps,qs) <- splits xs ] 
-- :t splits
-- returns splits :: [a] -> [([a],[a])]

