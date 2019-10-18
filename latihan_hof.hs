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