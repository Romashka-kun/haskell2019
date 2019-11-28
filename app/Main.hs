module Main where

import           Algorithms
import           Lec3
import           Lec4
import           Lec5
import           Lec6
import           Lib

main :: IO ()
main = do
  someFunc
--  print(foldr2 (+) 0 [1,2,3])
--
--  print(lengthL [1,2,3])
--  print(lengthR [1,2,3,4])
--
--  print(reverseL [1,2,3,4])
--
--  print(concatR ["a","b"] ["c", "d"])
--
--  print(filterR (\x -> mod x 2 == 0) [1,2,3,4,5])
--
--  print(mapR ('!':) ["1","2","3"])
--
--  print(concatMapL (\x -> ['!' : x, '?' : x]) ["a", "b", "c"])
--  print(concatMapR (\x -> ['!' : x, '?' : x]) ["a", "b", "c"])
--
--  print(foldl (-) 0 [1,2,3])
--  print(foldlR (-) 0 [1,2,3])
--
--  print(foldr (-) 0 [1,2,3])
--  print(foldrL (-) 0 [1,2,3])
--
--  print(total (Bill [("телефон", 100000), ("мороженое", 1000)]))
--
--  print(minmax [10, 20, 0, 4, -5])
--
--  print(repeatTwice [10, 20, 0, 4, -5])
--
--  print(filterCM (\x -> mod x 2 == 0) [1,2,3,4,5])
--
--  print(mapCM ('!':) ["a","b","c"])
--
--
--  print(exponent1 3 11)
--
--  print(quickSort [3,2,5,8,9,4,1,5,7])
--
--  print(vectorSum (Point 2 3) (Point 1 3))
--
--  print(sum1 (Cons 3 (Cons 5 (Cons 1 Empty))))
--  print(sum1 Empty)
--
--  print(length1 (Cons 3 (Cons 5 (Cons 1 Empty))))
--  print(length1 Empty)
--
--  print(reverse1 (Cons 3 (Cons 5 (Cons 1 (Cons 10 Empty)))))
--  print(reverse1 Empty)
--
--  print(mergeSort [6, 3, 1, 9, 5, 4, 7, 8, 2])
--  print (zipWithIndex [1, 2, 3, 4, 5])
--  print (indexSum [1, 2, 3, 4, 5])
--  print (evenToZero [1, 2, 3, 4, 5])
--  print (deleteEvenIndexes [1, 2, 3, 4, 5])
--  print (separateDigits [10, 44, 31, 93])
--  print (safePop [1, 2, 3, 4, 5])
--  print (safeGet 1 [1, 2, 3, 4, 5])
--
--  print (rev 42 :: Int)
--  print (rev [1, 2, 3, 4, 5])
--  print (rev ["a", "b", "c", "d"])
--
--  print (rev (Lec5.Point 24 13))
--  print (Lec5.Point 24 13 == Lec5.Point 24 13)
--  print (Lec5.Point 24 13 == Lec5.Point 2 3)
--
--  print (min (Lec5.Point 24 13) (Lec5.Point 2 3))
--  print (maximum [Lec5.Point 24 13, Lec5.Point 2 3, Lec5.Point 6 84, Lec5.Point 24 13])

  print (Node 5 (Node 3 EmptyTree EmptyTree) (Node 8 (Node 6 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree)))
  print
    (map'
       (++ "!")
       (Node "5"
          (Node "3" EmptyTree EmptyTree)
          (Node "8" (Node "6" EmptyTree EmptyTree) (Node "10" EmptyTree EmptyTree))))
          
  print 
    (appendTree 
        7
        (Node 5 (Node 3 EmptyTree EmptyTree) (Node 8 (Node 6 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))))

--  print (min' (Tree 5 (Tree 3 EmptyTree EmptyTree) (Tree 8 (Tree 6 EmptyTree EmptyTree) (Tree 1 EmptyTree EmptyTree))))