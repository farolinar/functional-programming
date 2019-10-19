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
