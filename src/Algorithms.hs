module Algorithms where

exponent1 :: Int -> Int -> Int
exponent1 x n = answer x n 1
  where
    answer a b c
      | b == 0 = c
      | mod b 2 == 0 = answer (a * a) (div b 2) c
      | otherwise = answer a (b - 1) (c * a)

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (x:l) = partition x l
  where
    partition v l = quickSort (filter (< v) l) ++ (v : filter (== v) l) ++ quickSort (filter (> v) l)

mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort l = merge (mergeSort part1) (mergeSort part2) []
  where
    (part1, part2) = splitList l
    splitList l = splitAt ((length l + 1) `div` 2) l
    merge [] b l = reverse l ++ b
    merge a [] l = reverse l ++ a
    merge (x:a) (y:b) l
      | x < y = merge a (y : b) (x : l)
      | otherwise = merge (x : a) b (y : l)
