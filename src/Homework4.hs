module Homework4(iter) where

iter :: (a -> a) -> Integer -> (a -> a)
iter _ 0 = id  -- Базовый случай: если n = 0, то вернуть функцию тождественности id
iter f n = f . iter f (n - 1)  -- Рекурсивный случай: применить функцию f и рекурсивно вызвать iter с уменьшением n на 1
