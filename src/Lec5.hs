module Lec5 where

-- Параметризованный алгоритмический тип
-- MyList = Empty | Cons Int MyList -- список элементов Int
data MyList a
  = Empty
  | Cons a (MyList a) -- список элементов типа a

--          ^типовая переменная
-- Cons "abc" Empty <- список строк
-- Тип Maybe
-- data Maybe a = Just a | Nothing
-- maybeHead :: [a] -> Maybe a
-- maybeHead [] = Nothing
-- maybeHead (x:_) = Just x
-- Как пользоваться:
-- f :: [Int] -> Int -- сумма первых двух элементов
-- f l = case maybeHead l of
--          Nothing -> 0
--          Just a -> a + (case maybeHead (tail l) of
--                            Nothing -> 0
--                            Just b -> b
--                        )
-- + невозможно использовать несуществующие значения
-- см. монады (оператор do)
--
-- data Either a b = Left a | Right b
-- Left - информация об объекте
-- Right - результат выполнения
-- Классы типов
-- функции работают с некоторыми типами
-- (+) - любые числа может сложить. Но не строки и ничего не похожее на числа.
-- Show - практически что угодно превращает в строку
-- show 42 - "42"
-- как описать +
-- (+) :: a -> a -> a, но только, если a - это число или Point, который мы ввели сами
-- сделаем свою функцию rev, она переворачивает объекты
-- создадим класс типов Reversible. Некоторые типы будут иметь этот класс, т.е. для них можно будет вызывать функцию rev
--
--class Reversible a where
--rev :: a -> a
-- теперь напишем rev для каких-нибудь типов
-- Int будет переворачиваемым
--instance Reversible Int
--rev :: Int -> Int
--rev = list2num . reverse . num2list
--  where
--    list2num = foldl (\a x -> 10 * a + x) 0
--    num2list 0 = []
--    num2list n = (n `mod` 10) : num2list (n `div` 10)
--    num2list n = h n []
--      where
--        h 0 l = l
--        h n l = h (n `div` 10) (n `mod` 10 : l)
-- 1) rev = map rev . reverse
-- 2) rev l = map rev (reverse l)
-- rev (42::Int)
-- rev [42::Int, 51]
-- rev [[42::Int, 51], [73]]
-- Как написать функцию, которая требует тип определенного класса?
-- 1) f x = rev (rev x)
-- 2) f = rev.rev
-- какой тип f :: Reversible a => a -> a
-- класс Num
-- class Num a where
-- (+) :: a -> a -> a
-- (-) :: a -> a -> a
-- (*) :: a -> a -> a
-- negate :: a -> a
-- abs :: a -> a
-- signum :: a -> a
-- fromInteger :: Integer -> a
-- class Num a => Fractional where
-- (/) :: a -> a -> a
-- проблема 2 / 3 -> нет проблем
-- (length l) / 2 - нельзя делить Int -> fromIntegral (length l)
--zip, zipWith, unzip
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

indexSum :: [Int] -> [Int]
indexSum = zipWith (+) [0 ..]

evenToZero :: [Int] -> [Int]
evenToZero l = helper $ zipWithIndex l
  where
    helper =
      map
        (\(a, b) ->
           if a `mod` 2 == 0
             then 0
             else b)

deleteEvenIndexes :: [Int] -> [Int]
deleteEvenIndexes l = res
  where
    helper = filter (\(a, b) -> a `mod` 2 /= 0)
    (_, res) = unzip $ helper $ zipWithIndex l

-- fst, snd
separateDigits :: [Int] -> ([Int], [Int])
separateDigits l = unzip $ map (`divMod` 10) l

-- Maybe
safePop :: [a] -> Maybe (a, [a])
safePop []    = Nothing
safePop (x:l) = Just (x, l)

safeGet :: Int -> [a] -> Maybe a
safeGet i l =
  case safePop l of
    Nothing -> Nothing
    Just (x, l) ->
      if i == 0
        then Just x
        else safeGet (i - 1) l