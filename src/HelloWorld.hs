module HelloWorld where
--Функция main - с неё запускается программа
--тип IO
--другие функци:
--	f::Int -> Int - не обязательно, но желательно указать тип
--	f x = x + 1
--или
--	g::Int -> Int -> Int
--	g 0 x = x
--	g x y = x
--	как работает g:   g 10 5 - применение функции через пробел, без скобок

main :: IO()

f :: Int -> Int
f x = x + 1

g :: Int -> Int -> Int
g 0 x = x
g x y = x

-- 1)
abs1 :: Int -> Int
abs1 x = if x < 0 then -x else x
--
-- 2)
abs2 :: Int -> Int
abs2 x | x < 0 = -x
	   | otherwise = x
--
-- 3)
sign1 :: Int -> Int
sign1 x | x < 0 = -1
		| x == 0 = 0
		| otherwise = 1
--
-- 4)
--fact :: Integer -> Integer
--fact 0 = 1
--fact n | n < 0 = 0
--fact n = n * fact (n - 1)
--
-- 	  fact 3 -> 3 * fact 2 -> 3 * 2 * fact 1 -> 3 * 2 * 1 * fact 0 -> 3 * 2 * 1 * 1
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)
main = print (fib 4)

--Улучшенный факториал
factHelper :: Integer -> Integer -> Integer
factHelper 0 f = f
factHelper n f = factHelper (n - 1) (f * n)  -- (f * n) - аккумулятор, копит ответ

fact :: Integer -> Integer
fact n = factHelper n 1 where 
						factHelper 0 f = f
		  				factHelper n f = factHelper (n - 1) (f * n)
--
factHelper2 :: Integer -> Integer -> Integer -> Integer
factHelper2 n m f | n == m = f
factHelper2 n i f = factHelper2 n (i + 1) f * (n + 1)
fact1 n = factHelper2 n 0 1
--
--Упр. реализовать fib иначе, чтобы работал эффективно
--
fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1Helper (n - 2) 1 1	where 
								fib1Helper 0 prev s = s
		  						fib1Helper n prev s = fib1Helper (n - 1) s (prev + s) 
--
--Создание списка
--[1, 2, 3] :: [Int]
--[42]
--[[1, 2], [1, 2, 3], []] :: [[Int]]
--
--Операция: -присоединение головы списка
--5: [1, 2, 3] -> [5, 1, 2, 3]
--1: [] -> [1]
--1:2:3:[] -> 1:2:[3] -> 1:[2, 3] -> [1, 2, 3]
--диапазоны:
--	[1..10] - список от 1 до 10
--	[1, 3..10] - 1, 3, 5, 7, 9
--	[1..] - список до бесконечности
-- ++ - соединение списков
-- length - длина
-- head - 1-й
-- last - последний
--
--Пример: сумма элементов списка
sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:l) = x + sum1 l
--
sum2 :: [Int] -> Int
sum2 l = sum2helper l 0
sum2helper [] a = a
sum2helper (x:l) a = sum2helper l (x + a)
--
--
fact2 n = product [1..n]
-- написать самостоятельно:
-- sum, product, maximum, minimum
--take :: Int -> [a] -> [a] - взять столько-то элементов списка
--drop :: Int -> [a] -> [a] - отбросить столько-то элементов списка
--[10, 20, 30] !! 2 -> 30
-- ** [String] -> [Int] -> [String] - выдаются строки с указанными индексами
--
drop1 :: Int -> [String] -> [String]
drop1 0 l = l
drop1 n (x:l) = drop1 (n - 1) l
--
--maximum1
--
maximum1 :: [Int] -> Int
maximum1 [x] = x
maximum1 (x:l) | x > maximum1 l = x
			   | otherwise = maximum1 l
--
--minimum1
--
minimum1 :: [Int] -> Int
minimum1 [x] = x
minimum1 (x:l) | x < minimum1 l = x
			   | otherwise = minimum1 l
--
--length1  0 tail rec
--
length1 :: [a] -> Int
length1 l = helper 0 l where
							   helper n [] = n
							   helper n (_:l) = helper (n + 1) l
--
--last1
--
last1 :: [a] -> a
last1 [x] = x
last1 (x:l) = last1 l
--
--push1   0 tail rec
--
push1 :: a -> [a] -> [a]
push1 c l = helper c l [] where
								helper c [] l2 = rev1 (c:l2)
								helper c (x:l1) l2 = helper c l1 (x:l2)
--
--conc1  1 optimize
--
conc1 :: [a] -> [a] -> [a]
--conc1 [] l2 = l2 
--conc1 (x:l1) l2 = x : conc1 l1 l2
conc1 l1 l2 = helper l1 l2 [] where
								    helper [] l2 [] = l2
								    helper [] l2 (x:l3) = helper [] (x:l2) l3
								    helper (x:l1) l2 l3 = helper l1 l2 (x:l3)
--
--repeat1   0 tail rec
--
repeat1 :: Int -> Char -> String
repeat1 n x = helper n x [] where
							    	helper 0 x l = l
							    	helper n x l = helper (n - 1) x (x:l) 
--
--get1
--
get1 :: Int -> [a] -> a
get1 0 (x:l) = x
get1 i (x:l) = get1 (i - 1) l
--
--rev1   1 optimize
--
rev1 :: [a] -> [a]
rev1 l = helper l [] where
						  helper [] l2 = l2
						  helper (x:l1) l2 = helper l1 (x:l2)
--
--take1   1 implement
--
take1 :: Int -> [a] -> [a]
take1 n l = helper n l [] where
							    helper 0 l1 l2 = rev1 l2
							    helper n (x:l1) l2 = helper (n - 1) l1 (x:l2)
--
-- **
--
dblStar :: [a] -> [Int] -> [a]
dblStar l1 [] = []
dblStar l1 (x:l2) = get1 x l1 : dblStar l1 l2
