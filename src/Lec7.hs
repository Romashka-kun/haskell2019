module Lec7 where

-- Функторы
--
-- Функция map была бы логична не только для списков. Как бы она работала для Maybe?
-- f x = x + 1
-- x1 = Just 42
-- x2 = Nothing
--
-- -хочу применить f к x1, x2, точнее, к значению внутри
-- maybeMap fun (Just n) = Just (fun n)
-- maybeMap _ Nothing = Nothing
-- print $ maybeMap f x1  -> Just 43
-- print $ maybeMap f x2  -> Nothing
--
-- Допустим, я хочу уметь так же применять функции ко вторым элементам пары:
-- f x = x + 1
-- x1 = ("hello", 42)
-- x2 = ([1, 2, 3], 42.0)
-- хочу применить f ко второму элементу
-- pairMap fun (x, y) = (x, fun y)
-- print $ pairMap f x1  -> ("hello", 43)
-- print $ pairMap f x2  -> ([1, 2, 3], 43.0)
--
-- Есть и другие аналогичные ситуации. Когда значение (или значения) хранятся в какой-то структуре с дополнительными
-- данными.
-- Список - несколько значений и информация об их порядке
-- Maybe - одно значение или ноль
-- Пара - значение вместе с дополнительной информацией
--
-- Функции тоже можно понимать как такие структуры:
-- fun :: a -> b
-- получается, функция хранит значение типа B, доступ к которым осуществляется по аргументу типа a:
-- Например,
-- f 1 = "один"
-- f 2 = "два"
-- функция "хранит" внутри себя значения "один", "два", получить их можно, подставив соответствующий аргумент в функцию.
-- f x = x + 1
-- x1 = \x -> 2 * x
-- x2 = \x -> 3 * x
-- funMap f x1 - должна получиться функция, которая сопадает с x1, но после применения x1 дополнительно вызывает f
-- funMap fun x = \t -> fun (x t)
--
-- xx1 = funMap f x1
-- xx2 = funMap f x2
-- print $ xx1 42  -> 85
-- print $ xx2 42  -> 127
--
-- На самом деле все эти map функции уже есть. Они называются fmap или <$>, и они делают ровно то же, что делаем мы:
-- print $ fmap (+1) (Just 42)  -> Just 43
-- print $ (+1) <$> Just 42  -> Just 43
-- print $ (+1) <$> [10, 20, 30]  -> [11, 21, 31]
-- print $ (+1) <$> ("abc", 123)  -? ("abc", 124)
--
-- Это работает, потому что в Haskell есть класс типов Functor - функтор. Он определен так:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- У него есть реализации, например, примерно так выглядит реализация для Maybe.
-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap _ Nothing = Nothing
--
-- Если вы определяете функтор, вы должны гарантировать выполнение следующего правила. Функция id - это функция, которая
-- ничего не делает: id x = x, её тип a -> a
-- **Правило**:
-- 1) fmap id x == x (или эквивалентно) fmap id == id
-- 2) fmap (g . f) x == fmap g . fmap f
--
-- ## Аппликативные функторы
-- Расширение обычных функторов дополнительными операциями. Представьте опять же Maybe.
-- Было в функторах:
-- 1) Обычная функция
-- 2) Данные внутри структуры
-- 3) Применяем функцию к каждому данному внутри структуры
-- Теперь в аппликативных функторах можно дополнительно:
-- 1) Обычные функция внутри структуры
-- 2) Данные внутри структуры
-- 3) Применяем функции из структуры к данным. Как именно применять - очень сильно зависит от ситуации.
--
-- Пример с Maybe:
-- f = Just (+1) -- увеличение на 1, но внутри Maybe
-- x = Just 42 -- значение внутри Maybe
-- применение:
-- maybeApply (Just fun) (Just x) = Just (fun x)
-- maybeApply _ _ = Nothing
--
-- print $ maybeApply f x  -> Just 43
-- print $ maybeApply Nothing x  -> Nothing
-- print $ maybeApply (Just (*2)) Nothing  -> Nothing
--
-- А для списков?
-- f = [(+1), (*2)]
-- x = [10, 20, 30]
-- что может означать listApply, когда есть список функций и список значений?
-- Применить каждую функцию к каждому элементу, получить список новых значений.
--
-- listApply fs xs = concatMap (\f -> map f xs) fs
-- print $ listApply f x  -> [11, 21, 31, 20, 40, 60]
-- Немного философии, почему для списков это логично делать именно так. Можно считать, что Maybe - это вычисления,
-- в которых иногда отсутствуют результаты. А в списках можно считать, что мы делаем вычисления, в которых может быть
-- несколько результатов.
-- В Haskell есть класс типов Аппликативные функторы:
-- class Functor f => Applicative f where
--   pure :: a -> f a -- вставить значение в функтор
--   (<*>) :: f (a -> b) -> f a -> f b -- та самая функция применения, рассмотренная выше
--   -- и ещё несколько, см. дальше
--
-- pure - это функция, которая помещает значение внутрь функции, "самым естественным способом". Что это значит будет
-- понятно из требований к аппликативному функтору, см. дальше.
-- Для Maybe: pure x = Just x, для списка pure x = [x]
--
-- Допустим, хотим сложить значения в двух Maybe. Или в двух списках сложить попарно значения. Как это будет написано?
-- x1 = Just 110
-- x2 = Just 654
-- надо сложить. Сначала давайте получим Just (+110), потом применим её к Just 654:
-- (+) <*> x1 <*> x2
--
-- Аппликативные функторы должны уметь делать не только <*>, но и функцию pure, которая вводит обычное значение в функтор.
-- 'pure 42' в типе Maybe это 'Just 42' . 'pure 42' в типе списка: [42]
--
-- Правила для функторов:
--   pure id <*> v = v
--   pure f <*> pure x = pure (f x)
--   и др.
--
--
-- #Монады
-- Вычисления с Maybe - это вычисления, в которых может не получиться результата. Если мы делаем последовательность
-- вычислений, в которой в какой-то момент результата не получилось, то и дальше считаем, что результата нет.
-- mSqrt :: Double -> Maybe Double
-- mSqrt x | x < 0 = Nothing
--         | otherwise = Just $ sqrt x
-- [хотим вычислить (sqrt(x) + 1) / 2]
-- eval x = let sx = mSqrt x
--              y = (+1) <$> sx
--            in (/2) <$> y
-- eval 9 -> Just 2.0
-- eval (-9) -> Nothing
--
-- sqrt(x) + sqrt(y) + 1
-- eval2 x y = let sx = mSqrt x
--                 sy = mSqrt y
--                 sq_plus_sq = Just (+) <*> sx <*> sy -- liftA2 (+) sx sy
--                in
--                 (+1) <$> sq_plus_sq
-- eval2 9 16 -> Just 8.0
-- eval2 (-9) 16 -> Nothing
--
-- sqrt(sqrt(x) - 3)
-- eval3 x = let sx = mSqrt x
--               sx_3 = (\t -> t - 3) <$> sx
--               -- ?? не получается сделать ни с <*>, ни с <$>
--               -- есть Just y и есть mSqrt :: Double -> Maybe Double
--               z = case sx_3 of
--                      Just n -> mSqrt n
--                      Nothing -> Nothing
--              in
--               z
-- eval3 100 -> Just ...
-- eval3 (-100) -> Nothing
-- eval3 4 -> Nothing
--
-- Хочется вспомогательную функцию типа <*> и <$>, чтобы она имела такой заголовок
-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b
--
-- Если подумать, то
-- 1) fmap - частный случай этой функции. Сравним
--    bind :: Maybe a -> (a -> Maybe b) -> Maybe b
--    fmap :: Maybe a -> (a -> b) -> Maybe b
--    fmap f t = bind t (\x -> pure $ f x) или fmap f t = bind t (pure . f)
-- 2) <*> - тоже частный случай. Попробуйте её сам выразить через bind
--
-- Реализуем bind для Maybe:
-- maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- maybeBind Nothing _ = Nothing
-- maybeBind (Just x) f = f x
--
-- снова sqrt(sqrt(x) - 3)
-- eval3 x = let sx = mSqrt(x)
--               sx_3 = maybeBind sx (\t -> Just (t - 3))
--              in
--               maybeBind sx_3 mSqrt
-- eval3 100 -> Just ...
-- eval3 (-100) -> Nothing
-- eval3 4 -> Nothing
--
-- короткий синтаксис
-- eval3 x = do
--             sx <- mSqrt x
--             sx_3 <- sx - 3
--             mSqrt sx_3
-- ответы те же
--
-- Давайте остановимся на одной вспомогательной функции
-- maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- Функции даётся значение, очередной шаг вычисления, она возвращает новое значение.
-- maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- maybeBind Nothing _ = Nothing
-- maybeBind (Just x) f = f x
--
-- взятие корня
-- mSqrt :: Double -> Maybe Double
-- mSqrt x | x < 0 = Nothing
--         | otherwise = Just $ sqrt x
-- sqrt(sqrt(x) - 3)
--
-- eval4 :: Double -> Maybe Double
-- maybeBind (Just x) mSqrt - это sqrt(x) внутри maybe
-- eval4 x = maybeBind (mSqrt x) (\a -> mSqrt (a - 3))
--
-- Эта программа похожа на последловательность шагов:
-- 1. mSqrt
-- 2. \a -> mSqrt (a - 3)
--
-- Посчитаем теперь sqrt x + sqrt y + 1
-- Шаги:
-- 1. sqrt x
-- 2. sqrt y
-- 3. sqrt x + sqrt y + 1
--
-- eval5 :: Double -> Double -> Maybe Double
-- eval5 x y = maybeBind (mSqrt x) (\a -> maybeBind (mSqrt y) (\b -> Just (a + b + 1)))
--
-- maybeBind - это функция '>>=', которая существует для Maybe и других типов.
-- eval5 :: Double -> Double -> Maybe Double
-- eval5 x y = (mSqrt x) >>= (\a -> (mSqrt y) >>= (\b -> Just (a + b + 1)))
--
-- для подообной записи есть do-нотация
-- eval5 :: Double -> Double -> Maybe Double
-- eval5 x y = do
--              a <- mSqrt x
--              b <- mSqrt y
--              return (a + b + 1)
-- Получился "императивный" код. return для Maybe это Just.
-- Перепишем теперь eval4
-- eval4 :: Double -> Maybe Double
-- eval4 x = (mSqrt x) >>= (\a -> mSqrt (a - 3))
-- или в do-нотации
-- eval4 x = do
--             a <- mSqrt x
--             mSqrt (a - 3) -- сразу возвращает Maybe
--
-- Комплексное число 1 :+ 2 это 1 + 2i
-- import Data.Complex
-- lSqrt :: Complex Double -> [Complex Double]
-- lSqrt x = [sqrt x, -sqrt x]
--
-- eval5 :: Complex Double -> Complex Double -> [Complex Double]
-- eval5 x y = do
--              a <- lSqrt x
--              b <- lSqrt y
--              return (a + b + 1)
--
-- Монада в хаскеле:
-- class Applicative a => Monad (m a) where
-- (>>=) :: m a -> (a -> m b) -> m b
-- return :: a -> m a -- превращает обычное значение в значение в монаде
-- (>>) :: m a -> m b -> m b
-- (>>) x y = x >>= (\_ -> y)
--
-- Пример монады. Вычисления с журналированием действий.
data Log a =
  Log a [String]
  deriving (Show) -- храним значение и список сообщений

-- Будем пользоваться функциями, которые при вычислениях могут выдать несколько сообщений:
add1 :: Int -> Log Int
add1 x = Log (x + 1) ["added 1"]

--
mul2 :: Int -> Log Int
mul2 x = Log (2 * x) ["multiplied by 2", "that was hard"]

--
-- Скажем, что Log это монада и объясним, как комбинировать вычисления.
instance Functor Log where
  fmap f (Log v msg) = Log (f v) msg

--
instance Applicative Log where
  pure x = Log x []
  Log f newMsg <*> Log v msg = Log (f v) (msg ++ newMsg)

--
instance Monad Log where
-- (>>=) :: Log a -> (a -> Log b) -> Log b
  (Log x messages) >>= f =
    let (Log y newMessages) = f x
      in Log y (messages ++ newMessages)

  -- return :: a -> Log a
  return x = Log x []

--
-- 2 + x + 1 через функции add1 и mul2
eval6 :: Int -> Log Int
eval6 x = do
  a <- mul2 x
  add1 a -- или b <- add1 a; return b
-- eval6 10 -- должен вернуть 21 и журнал ["умножила на 2", "добавила 1"]


--sumIO :: IO ()
--sumIO = do
--  a <- getLine
--  b <- getLine
--  print (read a + read b)
sumIO :: IO Int
sumIO = do
    a <- getLine
    b <- getLine
    return (read a + read b)