module Main where

import Lib
import Lec3
import Lec4
import Algorithms

main :: IO ()
main = do
  someFunc
  print(foldr2 (+) 0 [1,2,3])

  print(lengthL [1,2,3])
  print(lengthR [1,2,3,4])

  print(reverseL [1,2,3,4])
  
  print(concatR ["a","b"] ["c", "d"])

  print(filterR (\x -> mod x 2 == 0) [1,2,3,4,5])

  print(mapR ('!':) ["1","2","3"])

  print(concatMapL (\x -> ['!' : x, '?' : x]) ["a", "b", "c"])
  print(concatMapR (\x -> ['!' : x, '?' : x]) ["a", "b", "c"])

  print(foldl (-) 0 [1,2,3])
  print(foldlR (-) 0 [1,2,3])

  print(foldr (-) 0 [1,2,3])
  print(foldrL (-) 0 [1,2,3])

  print(total (Bill [("телефон", 100000), ("мороженое", 1000)]))

  print(minmax [10, 20, 0, 4, -5])

  print(repeatTwice [10, 20, 0, 4, -5])

  print(filterCM (\x -> mod x 2 == 0) [1,2,3,4,5])

  print(mapCM ('!':) ["a","b","c"])


  print(exponent1 3 11)
  
  print(quickSort [3,2,5,8,9,4,1,5,7])
  
  print(vectorSum (Point 2 3) (Point 1 3))