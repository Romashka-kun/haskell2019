module Lec3 where

-- Карринг
-- s :: Int -> Int -> Int
-- s x y = x + y
-- s 2 3 -> 5
-- f = s 2 - f - это функция Int -> Int
-- f 5 -> 7
--
-- f - частичное применение функции s, f - добавляет 2
-- 
-- Вспомним map: (L -> B) -> [L] -> [B]
-- map (\x -> x + 1) = g
-- g = map (\x -> x + 1) - частичное применение функции map
-- Теперь g - увеличение элементов списка на 1
--
--
-- Срезы
-- (-) :: Int -> Int -> Int
-- (1-) \
--       частичное применение функции 
-- (-1) / 
--
-- (1-) 5 -> -4
-- (-1) 5 -> 4
--
-- (++[5]) - функция приписывает [5] в конец списка
--
-- removeEven - пример описания функции, удалить четные из списка
-- removeEven :: [Int] -> [Int]
-- removeEven l = filter (\x -> mod x 2 /= 0) l
-- removeEven = filter (\x -> mod x 2 /= 0)
-- можно убирать одинаковые применения в конце
--
--
-- Функция $ применение
-- f $ x работает при f x
-- f <- что применять, x <- к чему
--
-- ($) :: (a -> b) -> a -> b
-- ($) f x = f x
-- Как использовать:
-- вместо f (n - 1) можно писать f $ n - 1
--        fact n f = fact (n - 1) (n * f)
--                 = fact $ n - 1 $ n * f
--
-- ($1) - функция, которая применяет заданную функцию к 1
--
--
-- Функция . - композиция
-- 
-- в математике: f(x) = sin x   f(g(x)) = sin (x + 1)
--               g(x) = x + 1   g(f(x)) = sin (x) + 1
--               h1 = f o g
--               h2 = g o f
--
-- Определим композицию
-- (.) :: (b -> y) -> (a -> b) -> (a -> y)
-- 1) (.) f g = p where p x = f (g x)
-- 2) (.) f g = \x -> f (g x)
-- 3) (.) f g x = f (g x)
--
-- Пример использования
-- map $ (+1).length $ ["abc", "x"]
--
--
-- Функции foldl, foldr
-- -универсальная обработка списков
-- некоторая функция применяется к каждому элементу списка и записывает всё в аккумулятор
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl (+) 0
-- sum l = foldl (+) 0 l
-- sum = foldl (+) 0
--
-- Опишем map через foldl
-- map f l = foldl (\a f -> a ++ [f x]) [] l - l можно убрать
--
--
-- foldr :: (a -> b -> b) -> b -> [a] -> b
--
-- foldl - поддерживает хвостовую рекурсию
-- foldr - нет

foldl2 :: (b -> a -> b) -> b -> [a] -> b
foldl2 f a [] = a
foldl2 f a (x:l) = foldl2 f (f a x) l

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f a [] = a
foldr2 f a (x:l) = f x (foldr2 f a l)

lengthL :: [a] -> Int
lengthL = foldl (\x _ -> x + 1) 0

lengthR :: [a] -> Int
lengthR = foldr (\_ x -> x + 1) 0

reverseL :: [a] -> [a]
reverseL = foldl (\l x -> x:l) []

--reverseR :: [a] -> [a]
--reverseR = foldr (\x l -> l ++ [x]) [] --плохо
--
--concatL :: [a] -> [a] -> [a]
--concatL = foldl (\l x -> l ++ [x]) --плохо

concatR :: [a] -> [a] -> [a]
concatR l1 l2 = foldr (:) l2 l1

--filterL :: (a -> Bool) -> [a] -> [a]
--filterL f = foldl (\a x -> if f x then a ++ [x] else a) [] --плохо

filterR :: (a -> Bool) -> [a] -> [a]
filterR f = foldr (\x a -> if f x then x : a else a) []

--mapL :: (a -> b) -> [a] -> [b]
--mapL f = foldl (\a x -> a ++ [f x]) []

mapR :: (a -> b) -> [a] -> [b]
mapR f = foldr ((:) . f) []

concatMapL :: (a -> [b]) -> [a] -> [b]
concatMapL f = foldl (\a x -> a ++ f x) []

concatMapR :: (a -> [b]) -> [a] -> [b]
concatMapR f = foldr ((++) . f) []

foldlR :: (b -> a -> b) -> b -> [a] -> b
foldlR f a l = foldr (flip f) a (reverse l)  -- todo to think

foldrL :: (a -> b -> b) -> b -> [a] -> b
foldrL f a l = foldl (flip f) a (reverse l)