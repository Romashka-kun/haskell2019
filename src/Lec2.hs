module Lec2 where

--Инфиксная/префиксная запись функции
--
--func a b <- функция от двух аргументов
--
--func a b <=> a func b
-- ^инфикс		^префикс
--
--если функция состоит из небукв, то она инфексная
--($>@) a b <=> a $>@ b
--
--(@) :: Int -> Int -> Int
--x @ y = x + x * y или (@) x y = x + x * y
--
--
--Параметрический полиморфизм
--length любой список -> длина
--при описании функции можно использовать типовые пременные (с маленькой буквы)
--
--
--Оператор case .. of - расширенный if
--case l of
--	[] -> "empty"
--	_ -> "not empty"
--
--
--Функции внешних порядков
--это функции, которые принимают (или возвращают) другие функции как аргументы
--
--Примеры:
--функция, которая применяет заданную функцию два раза
--app2 :: (Int -> Int) -> Int -> Int
--app2 f x = f (f x)
--
--f :: Int -> Int
--f x = x + 1
--app2 f 5 -> 7
--
--
--
--map - функция для работы со списком
--применяет указанную функцию по всем элементам списка
--Примеры:
--plus1 x = x + 1
--map plus1 [10,20,30] -> [11,21,31]
--sq x = x * x
--map sq [1,2,3] -> [1,4,9]
--map i ["abc", "xyz"] where i s = 'i':s -> ["iabc", "ixyz"]
--i `map` ["abc", "xyz"]
map1 :: (a -> b) -> [a] -> [b]
map1 _ []    = []
map1 f (x:l) = f x : map f l

--
--
--
--Ещё одна функция обработки списков
--filter :: (a -> Bool) -> [a] -> [a] -- список, в котором остались элементы со значением True
--filter p [10,20,30] where p x = x > 30 -> [30]
--
--
--
--Лямбда-выражения
-- \x -> x + 1
-- \x y -> x + y
--
--Принцип использования
--map (\x -> x + 1) [10,20] -> [11,21]
--map (\s -> 'i':s) ["abc", "xyz"] -> ["iabc", "ixyz"]
--
--Упражнение: самим реализовать filter
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x:l)
  | f x = x : f1
  | otherwise = f1
  where
    f1 = filter1 f l
   -- = if f x then x : f1 else f1 where f1 = filter1 f l

dividers :: Int -> [Int]
dividers n = filter (\d -> mod n d == 0) [1 .. n]

isPrime :: Int -> Bool
isPrime n = (n /= 1) && ((dividers n) !! 1 == n)
    -- | (dividers n) !! 1 == n = True
    -- | otherwise = False

isPerfect :: Int -> Bool
isPerfect n = sum (dividers n) - n == n
    -- | sum (dividers (n - 1)) == n = True
    -- | otherwise = False

million = filter isPerfect [1 .. 5000]

eratosthenes :: Int -> [Int]
eratosthenes n = helper [2 .. n]
  where
    helper []    = []
    helper (x:l) = x : helper (filter (\d -> mod d x /= 0) l)

twinPrime :: Int -> [[Int]]
twinPrime n = helper (eratosthenes n) 1
  where
    helper [] y = []
    helper (x:l) y
      | x - y == 2 = [y, x] : helper l x
      | otherwise = helper l x
