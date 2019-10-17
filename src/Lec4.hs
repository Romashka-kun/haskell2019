module Lec4 where

{-
 тип Tuple - хранит несколько значений, возможно, разных типов
 (Int, String) - это Tuple из двух значений
 (2, "abc") - скобки и запятая создают значения Tuple

 f :: (Int, Int) -> Int
 f (a, b) = a + b
 f (2, 5) = 7

 divmod :: Int -> Int -> (Int, Int)
 divmod x y = (div x y, mod x y)

 g :: Int -> Int -> Int
 g x y = a + b where (a, b) = divmod a b - механизм сопоставления с образцом срабатывает и в присваиваниях

 встроенная функция zip
 zip :: [a] -> [b] -> [(a, b)]
 пример: zip [1, 2, 3] -> [True, False, True] -> [(1, True), (2, False), (3, True)]
 unzip: :: [(a, b)] -> ([a], [b])


 Алгебраические типы данных

 пример:
 data Bool = True | False
 имя типа^    ^конструкторы типа
 пол человека:
 Sex = Male | Female | Other
 Figure = Rect Int Int | Circle Int
  конструктор^   ^тип аргументов
 Создать значение: Rect 2 3   -- Rect :: Int -> Int -> Figure
 площадь фигуры:
 area :: Figure -> Float
 area (Rect w h) = w * h
 area (Circle r) = r * r * pi

 использует area f where f = Rect 2 3
            или
            area (Rect 2 3)

 data Bill = Bill [(String, Int)]
     тип^     ^конструктор типа
 Bill [("телефон", 100000), ("мороженое", 1000)]  --значение типа Bill

-}

newtype Bill = Bill [(String, Int)] deriving Show
total :: Bill -> Int
total (Bill bill) = sum (map snd bill)

minmax :: [Int] -> (Int, Int)
minmax l = (minimum l, maximum l) -- todo one list traversal

repeatTwice :: [a] -> [a]
repeatTwice = concatMap (\x -> [x, x])

filterCM :: (a -> Bool) -> [a] -> [a]
filterCM f = concatMap (\x -> if f x then [x] else [])

mapCM :: (a -> b) -> [a] -> [b]
mapCM f = concatMap (\x -> [f x])


-- Алгебраические типы данных --

data Point = Point Int Int deriving Show

vectorSum :: Point -> Point -> Point
vectorSum (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

vectorLength :: Point -> Double
vectorLength (Point x y) = sqrt $ fromIntegral (x * x + y * y)


data Figure = Circle Double | Rect Double Double | Triangle Double deriving Show

area :: Figure -> Double
area (Circle r) = r * r * pi
area (Rect w h) = w * h
area (Triangle a) = a * a * sqrt 3 / 4


data MyList = Empty | Cons Int MyList deriving Show

sum1 :: MyList -> Int
sum1 Empty = 0
sum1 (Cons x l) = x + sum1 l

length1 :: MyList -> Int
length1 l = helper l 0
            where helper Empty acc = acc
                  helper (Cons x l) acc = helper l (acc + 1)

reverse1 :: MyList -> MyList
reverse1 l = helper l Empty
             where helper Empty acc = acc
                   helper (Cons x l) acc = helper l (Cons x acc) 