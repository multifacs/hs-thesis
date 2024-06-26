import Prelude hiding (minimum, repeat, replicate)

-------------------------------------------------
-- Конспект лекции 3 20.02.2023
-------------------------------------------------

-- Содержание

-- 1. Основы лямбда-исчисления с простыми типами
-- 2. Ленивые вычисления
-- 3. Арифметические прогрессии
-- 4. Генераторы списков

-- Напоминание: хорошим введением в Haskell является книга
-- Макеева, ссылка на которую находится на source.unn.ru.

-------------------------------------------------
-- 2. Основы лямбда-исчисления с простыми типами
-------------------------------------------------

-- Из Prelude

-- const :: a -> b -> a
-- const x _ = x

-- Функция, которая принимает аргументы типов a1, ..., an и возвращает
-- аргумент типа b, имеет тип a1 -> ... -> an -> b. Однако это не есть
-- примитивное выражение. На самом деле a1 -> a2 -> ... -> an -> b =
-- a1 -> (a2 -> (... -> (an -> b)...)). Таким образом, типы
-- определяются рекурсивно:
-- (1) Int, Char, Bool, ... -- это типы;
-- (2) если a и b -- типы, то a -> b -- также тип.
-- Это упрощение, так как в Haskell есть также полиморфные типы,
-- алгебраические типы, классы типов и др.

-- Значит, любая функция в Haskell есть функция одного аргумента. Но
-- она может возвращать функцию, которая принимает следующий аргумент
-- и т.д. Для того, чтобы это объяснить более подробно, полезно
-- научиться отличать по записи выражения, которые можно напечатать,
-- от функций. Пусть, например, x :: Int и y :: Int. Тогда y^x :: Int.
-- (Напоминание: ^ обозначает возведение в степень.)
-- Это выражение можно рассмотривать как функцию от y типа Int -> Int.
-- Эта функция записывается \y -> y^x. При фиксированном x она
-- принимает y и возвращает y^x. Например, при x = 2 это функция
-- возведения в квадрат. Если x :: Int, то \y -> y^x :: Int -> Int.
-- Аналогично можно рассмотреть функцию \x -> y^x :: Int -> Int при
-- фиксированном y :: Int. Это функция, возвращающая произвольную
-- степень y. Наконец, y^x можно рассмотреть как функцию и от x, и от
-- y: она записывается \x -> \y -> y^x, или сокращенно \x y -> y^x.
-- Это функция, которая принимает x и возвращает функцию, которая
-- превращает y в y^x. Таким образом, \x y -> y^x :: Int -> (Int -> Int).

-- Отличие в синтаксисе обычного выражения от функции является
-- основной идеей лямбда-исчисления -- математического аппарата,
-- на котором основан любой язык функционального программирования.
-- Лямбда-исчисление разработал американский математик Алонзо Чёрч.

-- Определение const выше эквивалентно следующим.

-- const x = \y -> x
-- const = \x y -> x
-- const = \x -> \y -> x

-- Это функция, которая принимает x и возвращает функцию-константу,
-- которая на любом аргументе возвращает x.

-------------------------------------------------
-- 3. Ленивые вычисления
-------------------------------------------------

-- undefined :: a

-- Это значение, вычисление которого вызывает исключение. Здесь a на
-- самом деле означает ∀a. a и означает, что undefined может иметь
-- любой тип. Таким образом, undefined можно помещать в любой
-- контекст, и это не приводит к ошибкам типа во время исполнения,
-- потому что undefined не возвращает никакого значения.

-- error :: [Char] -> a

-- Принимает сообщение об ошибке и вызывает исключение, которое
-- печатает это сообщение. Также возвращает произвольный тип, что
-- нормально, так как error не возвращает значение.

-- В отличие от большинства популярных языков программирования Haskell
-- использует ленивую стратегию вычисления. Выражение вычисляется
-- только тогда, когда его значение требуется, чтобы определить
-- дальнейший ход выполнения программы, или когда его надо напечатать.

-- Следующие вызовы не вызывают исключение, потому что ненужные
-- подвыражения не вычисляются.

-- const "hello" undefined
-- length [undefined]
-- let f (_, _) = 1 in f (undefined, 2)

-- Ленивая стратегия также позволяет строить бесконечные структуры.

-- Бесконечный список из x

repeat :: a -> [a]
repeat x = x : repeat x

-- take n l возвращает первые n элементов списка l
-- take 5 (repeat 1)

-- replicate n x возвращает список длины n, состоящий из x.
-- replicate через repeat

replicate n x = take n (repeat x)

naturals = naturalsFrom 1
naturalsFrom n = n : naturalsFrom (n + 1)

factorial n = product (take n naturals)

-- Что будет, если функции дать бесконечный список?

-- 1. Вернет конечный результат. Пример: take

-- 2. Вернет бесконечный список, с которым можно работать,
--    если не вычислять его целиком.
--    Пример: take 3 (drop 2 naturals)

-- 3. Не остановится. Пример: length naturals

-- Вычисление выражения в интерпретаторе вызывает функцию show, которая
-- преобразовывает полученное значение в строчку. Значение
-- анализируется целиком, поэтому вычисление в командной строке
-- (at the prompt) энергичное, а не ленивое.

-------------------------------------------------
-- 4. Арифметические прогрессии
-------------------------------------------------

-- [1..10] -> [1,2,3,4,5,6,7,8,9,10]
-- [1,3..10] -> [1,3,5,7,9]
-- [10,9..1] -> [10,9,8,7,6,5,4,3,2,1]
-- [1..] -> [1,2,3,4,5,6,7,8,9,10,...]

-- Начало, разность и конец не обязаны быть константами

arithSeq start diff end = [start, start + diff .. end]

-- работает также с символами
-- ['A'..'Z']
-- и с Float и Double (разность по умолчанию равна 1)
-- В общем случае эта запись работает с типами, принадлежащими классу типов Enum

-- Еще один вариант факториала

-- factorial n = product [1..n]

-- Вообще написание различных версий факториала является национальным
-- спортом программистов на Haskell.

-------------------------------------------------
-- 5. Генераторы списков
-------------------------------------------------

-- Также используются названия: замыкания списков, абстракция списков,
-- списковое включение. Англ. list comprehension, по аналогии с set
-- comprehension.

-- > [x^2 | x <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]

-- > [(x,y) | x <- [1..4], y <- [1..4], x < y]
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- В общем случае:
-- [выражение | образец <- список, образец <- список, ... , условие, условие, ...]

-- Пример: быстрая сортировка

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
  qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]

-- Следующее определение проще написать, чем понять, как оно работает.
-- Во всяком случае, оно правильно с математической точки зрения.

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]
-- или
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- 0  1  1  2  3   5   8  13  21  34  55
-- 1  1  2  3  5   8  13  21  34  55 ...
----------------------------------------
-- 1  2  3  5  8  13  21  34  55  89 ...

-------------------------------------------------
-- Решение домашнего задания 2
-------------------------------------------------

-- 1. Числа Фибоначчи.
-- Нехвостовая рекурсия.

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Хвостовая рекурсия.
-- Два последовательных числа Фибоначчи передаются как второй и третий аргументы.
-- Первый аргумент является счетчиком.
-- Трассировка:

-- fib 5 0 1 =
-- fib 4 1 1 =
-- fib 3 1 2 =
-- fib 2 2 3 =
-- fib 1 3 5 =
-- fib 0 5 8 =
-- 5

-- Общий вид: fib n f_n f_{n+1}

fibIter :: (Eq t1, Num t1, Num t2) => t1 -> t2
fibIter n = fib n 0 1 where
  fib 0 x _ = x
  fib n x y = fib (n - 1) y (x + y)

-- 2. Напишите функцию merge :: [Int] -> [Int] -> [Int], которая сливает
-- два упорядоченных списка в один.

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge l1@(x : xs) l2@(y : ys)
  | x < y = x : merge xs l2
  | otherwise = y : merge l1 ys

-- 3. Напишите функцию halve :: [a] -> [([a], [a])], которая разбивает
-- данный список на две половины, длины которых отличаются не более,
-- чем на единицу. В определении можно использовать стандартные
-- функции. См. особенно функции take, drop и splitAt в описании Prelude.

halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

-- 4. Используя merge и halve, напишите функцию msort :: [Int] -> [Int],
-- реализующую сортировку слиянием.

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs =
  let (l1, l2) = halve xs in
    merge (msort l1) (msort l2)

-------------------------------------------------
-- Домашнее задание 3
-------------------------------------------------

-- Напишите модуль Homework3 в одноименном файле. Модуль должен
-- экспортировать только функции powerset, minimum, regLengths и
-- primes (см. ниже). Внутри модуля импортируйте Prelude за
-- исключением minimum.

-- 1. Используя генератор списков, напишите функцию powerset :: [a] ->
-- [[a]], которая возвращает список всех подсписков данного списка.
-- Порядок элементов в возвращаемых списках неважен. Например:

-- powerset [2, 3] = [[],[3],[2],[2,3]]
-- powerset [1,2,3] = [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- 2. Конъюнкция и дизъюнкция реализованы в Prelude следующим образом.

-- (&&)        :: Bool -> Bool -> Bool
-- True  && x  =  x
-- False && _  =  False
--              
-- (||)        :: Bool -> Bool -> Bool
-- True  || _  =  True
-- False || x  =  x

-- Объясните, как будут вычисляться выражения e1 && e2 и e1 || e2 для
-- произвольных выражений e1, e2 и сравните это с вычислением аналогичнх
-- выражений в других языках программирования, таких как C, Java и Python.

-- 3. Функцию minimum можно определить следующим образом.

minimum :: Ord a => [a] -> a
minimum ls = head (msort ls)

-- где msort нужно было написать в домашнем задании 2 (см. ниже). На
-- первый взгляд, это плохая идея, так как msort имеет сложность
-- O(n*log(n)), в то время как minimum должен иметь сложность O(n).
-- Однако из-за ленивой стратегии исполнения Haskell будет выполнять
-- только те сравнения, которые необходимы для получения головы
-- отсортированного списка. Рассмотрим вызов head (msort [2, 9, 1, 6, 4]).
-- Нарисуйте дерево рекурсивных вызовов функции msort и найдите
-- количество выполненных сравнений в функции merge. Определите
-- сложность такого определения minimum в Haskell.

-- 4. Можно доказать, что множество длин всех слов регулярного (или
-- конечно-автоматного) языка есть объединение конечного числа
-- арифметических прогрессий. Используя функцию merge, напишите
-- функцию regLengths :: [(Integer, Integer)] -> [Integer], которая
-- принимает список пар [(a1, d1), ..., (an, dn)] и возвращает
-- объединение арифметических прогрессий с первым членом ai и шагом di
-- для всех i = 1, ..., n. Полученный список должен быть отсортирован
-- по возрастанию и может содержать одинаковые элементы.

-- 5. Напишите функцию primes :: [Integer], которая вычисляет бесконечный
-- список простых чисел с помощью алгоритма "Решето Эратосфена" (см.
-- https://ru.wikipedia.org/wiki/Решето_Эратосфена). Начинать следует
-- с бесконечного списка нечетных чисел, больших 1. Для прореживания
-- списка используйте генератор списков.
