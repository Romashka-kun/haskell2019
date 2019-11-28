module Lec6 where

-- Ленивые вычисления
-- вычисления совершаются только тогда, когда они нужны
-- loop :: a
-- loop = loop - зависнет при вызове
-- вычисляем:
-- 2 == 3 && loop -> False
-- 2 == 2 && loop -> зависает
-- второй аргумент && не вычисляется, если первый ложь (см. short-circuit логические операторы)
-- head [2, loop, loop] -> 2
-- [2, loop, loop] !! 2 -> зависает
-- Haskell хранит не значения, а выражения для вычисления значения.
-- когда происходит вычисление?
-- 1) при печати на экран
-- 2) аргумент встроенной арифметиеской операции, типа +
-- 3) строгое применение  f $ x = f x
--                        f $! x = f x, где x сначала вычислится
-- пункты 2 и 3 -> 4) у некоторых встроенных функций аргументы могут быть строгими, т.е. вычисляются до применения
-- 5) data Pair a b = Pair a !b deriving Show - пара элементов
-- 6) при pattern matching
--
-- Pair 2 3 -> Pair 2 3
-- Pair (2 + loop) 3 -> Pair и зависает
-- Pair 3 (2 + loop) -> сразу зависает, потому что второй аргумент строгий и вычисляется сразу
-- пример 6):
-- f заменяет первый элемент списка на ноль
-- а) f :: [Int] -> [Int]
--    f (_:l) = 0:l
-- б) fp :: [Int] -> [Int]
--    fp l@(0:_) = l
--    fp (_:l) = 0:l
-- f [loop, 2, 3] -> [0, 2, 3]
-- fp [loop, 2, 3] -> зависает
-- isPrime n = (divsList n) !! 1 == n -> divsList вычисляет только первые два делителя
-- бесконечные списки
-- ones :: [Int]
-- ones = 1:ones -> список из 1
--
-- fib :: [Int]
-- fib = 1:1:(zipWith (+) fib (tail fib))
-- Двоичное дерево
-- Tree
data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show tree = drawTree tree "  "
    where
      drawTree EmptyTree _ = "EmptyTree"
      drawTree (Node val b1 b2) indent =
        show val ++ "\n" ++ indent ++ drawTree b1 (indent ++ "  ") ++ "\n" ++ indent ++ drawTree b2 (indent ++ "  ")

map' :: (a -> b) -> Tree a -> Tree b
map' _ EmptyTree        = EmptyTree
map' f (Node val b1 b2) = Node (f val) (map' f b1) (map' f b2)

appendTree :: Ord a => a -> Tree a -> Tree a
appendTree newNode EmptyTree = Node newNode EmptyTree EmptyTree
appendTree newNode (Node val b1 b2)
  | newNode > val = Node val b1 (appendTree newNode b2)
  | otherwise = Node val (appendTree newNode b1) b2
--min' :: Ord a => Tree a -> a
--min' (Tree node b1 b2)
--    | node < (min b1 b2) = node
--    | otherwise = min b1 b2
--  where
--    findMin min t1 t2 =
--foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
--   EmptyTree^     ^Node
