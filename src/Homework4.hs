module Homework4(iterate) where

iterate :: (a -> a) -> a -> [a]
iterate f x == [x, f x, f (f x), ...]