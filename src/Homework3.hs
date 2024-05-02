module Homework3(powerset, minimum2, primes, regLengths) where

import Homework2(msort)

minimum2 :: Ord a => [a] -> a
minimum2 ls = head (msort ls)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

regLengths :: [(Integer, Integer)] -> [Integer]
regLengths [] = []
regLengths [(a, d)] = [a, a + d ..]
regLengths ((a, d) : rest) = merge [a, a + d ..] (regLengths rest)