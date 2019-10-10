module Algorithms where

exponent1 :: Int -> Int -> Int
exponent1 x n = answer x n 1
                where answer a b c
                        | b == 0 = c
                        | mod b 2 == 0 = answer (a * a) (div b 2) c
                        | otherwise = answer a (b - 1) (c * a)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:l) = partition x l
                   where partition :: Int -> [Int] -> [Int]
                         partition v l = quickSort (filter (< v) l) ++ (v : filter (== v) l) ++ quickSort (filter (> v) l)