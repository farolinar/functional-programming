import Data.List;

lengthFunc [] = 0
--lengthFunc (x:xs) = 1 + (map (+1) xs)
--lengthFunc (x:xs) = 1 + lengthFunc xs
--lengthFunc (x:xs) = map (+1) xs
-- lengthFunc [] = []
--lengthFunc ls = (map (\x -> 1) ls)
lengthFunc ls = sum(map (\x -> 1) ls)

-- percobaan dengan currying:
--lengthFunc = sum(map (\x -> 1))
--Gagal

-- :t lengthFunc
-- returns lengthFunc :: Num p => [a] -> p

mysterious (x:xs) = map (+1) (map (+1) xs)
-- menambahkan elemen pada list kecuali elemen pertama pada xs dengan 1 sebanyak dua kali.

--iter 0 f x = (f x)

--iter n f x = map (\s -> f(iter s-1 f x)) [n, n-1..1]
--iter n f x = (iter n-1 f x) where n > 1
--iter :: Integral n => n -> ( a -> a ) -> a -> a 
iter 0 f x = x

iter n f x = f (iter (n-1) f x)

mystery = (\n -> iter n succ) 2 3
-- mystery equals to iter 2 (+1) 3 

-- [ x+1 | x <- xs ]
hof1a (x:xs) = map (+1) (x:xs) 

-- [ x+y | x <- xs, y <- ys ]

-- [ x+4 | (x,y) <- xys, x+y < 5 ]
list1e xys = [ x+4 | (x,y) <- xys, x+y < 5 ]
--hof1e xys = map (\(x,y) -> x+4) (filter (<5) xys)
hof1e xys = filter (<5) (map (\(x,y) -> x+4) xys)

--hoftemp xs ys = concat(map(\x -> map(\y -> x + y) ys) xs)

add [] [] = []
add (x:xs) (y:ys) = (x+y) : add xs ys

filterTemp xs ys = concat(map (\x -> map (\y -> (x,y)) ys) xs)