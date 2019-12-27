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
  | TreeNode a (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show tree = drawTree tree "  "
    where
      drawTree EmptyTree _ = "EmptyTree"
      drawTree (TreeNode val l r) indent =
        show val ++ "\n" ++ indent ++ drawTree l (indent ++ "  ") ++ "\n" ++ indent ++ drawTree r (indent ++ "  ")

instance Functor Tree where
  fmap f (TreeNode val l r) = TreeNode (f val) (fmap f l) (fmap f r)
  fmap _ EmptyTree          = EmptyTree

appendTree :: Ord a => a -> Tree a -> Tree a
appendTree newNode EmptyTree = TreeNode newNode EmptyTree EmptyTree
appendTree newNode (TreeNode val l r)
  | newNode > val = TreeNode val l (appendTree newNode r)
  | otherwise = TreeNode val (appendTree newNode l) r

min' :: Ord a => Tree a -> a
min' (TreeNode val EmptyTree _) = val
min' (TreeNode val l _)         = min' l

deleteMin :: Tree a -> Tree a
-- deleteMin EmptyTree = EmptyTree
deleteMin (TreeNode val EmptyTree r) = r
deleteMin (TreeNode val l r)         = TreeNode val (deleteMin l) r

-- LeafTree
data LeafTree a
  = Leaf a
  | InnerNode (LeafTree a) (LeafTree a)

instance (Show a) => Show (LeafTree a) where
  show tree = drawTree tree "  "
    where
      drawTree (Leaf l) _ = show l
      drawTree (InnerNode l r) indent =
        "○" ++ "\n" ++ indent ++ drawTree l (indent ++ "  ") ++ "\n" ++ indent ++ drawTree r (indent ++ "  ")

instance Functor LeafTree where
  fmap f (InnerNode l r) = InnerNode (fmap f l) (fmap f r)
  fmap f (Leaf val)      = Leaf (f val)

instance Applicative LeafTree where
  pure = Leaf
  ftree <*> InnerNode l r = InnerNode (ftree <*> l) (ftree <*> r)
  ftree <*> Leaf val = fmap ($ val) ftree
  
-- ├──
-- │   └──
sumMaybeInt :: Maybe Int -> Maybe Int -> Maybe Int
--sumMaybeInt a b = pure (+) <*> a <*> b
sumMaybeInt a b = (+) <$> a <*> b

sumList :: [Int] -> [Int] -> [Int]
sumList a b = pure (+) <*> a <*> b
