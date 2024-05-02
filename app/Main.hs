module Main (main) where

import Homework1
import Homework2
import Homework3
import Homework4
import System.Clock (toNanoSecs)

main :: IO ()
main = do
  putStrLn "Enter an integer:"
  input <- getLine
  case reads input of
    [(x, "")] -> putStrLn $ "You entered: " ++ show (x :: Int)
    _ -> putStrLn "Invalid input. Please enter an integer."

  let x = read input :: Integer

  time <- measureTime $ print ("fib = " ++ show (fibonacci x))
  putStrLn $ "Time taken: " ++ show (div (toNanoSecs time) (10 ^ 9)) ++ " s"

  let a = [1, 2, 3 :: Integer]
  let b = [4, 5, 6 :: Integer]
  print (mergeLists a b)
  print (mergeLists2 a b)

  let c = [1, 2, 3, 4 :: Integer]
  let d = [1, 2, 3, 4, 5 :: Integer]
  print (halve c)
  print (halve d)

  let e = [5, 4, 3, 2, 1 :: Integer]
  print (msort e)

  print (take 10 primes)

  print (iterate (+) 5)
