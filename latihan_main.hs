import Data.List

pembagi x n = (n `mod` x == 0)

divisor n = [x | x <- [ 1 .. n], pembagi x n]
-- pada mod digunakan tanda (`) bukan tanda petik satu (')
-- divisor mengembalikan list angka-angka yang dapat membagi habis n

multList [] = 1

multList (x:xs) = x * multList xs
-- mengalikan semua bilangan pada list

lebihkecillazy n = [ x | x <- [1 .. ], x < n]
--lazy evaluation tidak melakukan evaluasi pada hal yang tidak diperlukan
-- sisa elemen pada x yang melebihi n tidak akan diperhatikan sampai diperlukan.

quickSort [] = []

quickSort (x:xs) = [y| y <- xs, y <= x] ++ [x] ++ [y| y <- xs, y > x]
-- kesalahan tidak melakukan rekursif ulang sehingga ketika dimasukkan [5,4,6,8,9,2,2] menghasilkan [4,2,2,5,6,8,9]

-- perbaikan:
quickSortNew [] = []

quickSortNew (x:xs) = quickSortNew [y| y <- xs, y <= x] ++ [x] ++ quickSortNew [y| y <- xs, y > x]

-- penggunaan filter pada quickSort:
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
    where
        lesser = filter (< x) xs
        greater = filter (>= x) xs

-- setelah ini diketahui bahwa haskell case sensitive karena quickSort atas dan quicksort bawah berbeda

-- MERGE SORT coba di sini --


perm [] = [[]]

perm ls = [x:ps | x <- ls, ps <- perm(ls \\ [x])]
-- jalankan isi ls satu per satu dari depan, lalu permutasi sisanya

add [] [] = []

add (a:as) (b:bs) = (a+b) : (add as bs)

-- List ++ List => List
-- a : b => [a,b]

fibs = 1 : 1 : add fibs (tail fibs)

-- fibs      =  1 1 2 3 5 8 13 21
-- tail fibs =  1 2 3 5 8 13 21 

takes n ls = [ls!!x | x <- [0..n-1]]
-- membuat fungsi take sendiri karena pada saat pengerjaan lupa dengan fungsi take bawaan

primes = sieve [2..]
  where sieve (x:xs) = x : [y| y <- xs, y `mod` x /= 0]

-- mencoba primes:
tryPrime = takes 5 primes

-- trial: pada awal mencoba lupa menambahkan 'x:' sehingga hasil dari tryPrime adalah [3,5,7,9,11]