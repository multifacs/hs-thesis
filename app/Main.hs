module Main (main) where

import Homework1
import Homework2
import Homework3
import Homework4
import System.Clock (toNanoSecs)

import Data.List

type Id = (Char, Int)

data Term =
    Const Int
  | Var Id
  | Lambda Id Term
  | App Term Term
  | Plus Term Term
    deriving Show

t1 :: Term
t1 = (Lambda ('x', 1)
      (App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 2))))))

t2 :: Term
t2 = (Lambda ('x', 1)
      (App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 1))))))

freeOccurrences :: Char -> Term -> [Int]
freeOccurrences c (Const _) = []
freeOccurrences c (Var (c', i))
  | c == c'   = [i]
  | otherwise = []
freeOccurrences c (Lambda (c', i) term)
  | c == c'   = filter (/= i) (freeOccurrences c term)
  | otherwise = freeOccurrences c term
freeOccurrences c (App term1 term2) =
  freeOccurrences c term1 ++ freeOccurrences c term2
freeOccurrences c (Plus term1 term2) =
  freeOccurrences c term1 ++ freeOccurrences c term2

boundOccurrences :: Char -> Term -> [Int]
boundOccurrences c (Const _) = []
boundOccurrences c (Var (c', i)) = []
boundOccurrences c (Lambda (c', i) term)
  | c == c'   = i : boundOccurrences c term
  | otherwise = boundOccurrences c term
boundOccurrences c (App term1 term2) =
  boundOccurrences c term1 ++ boundOccurrences c term2
boundOccurrences c (Plus term1 term2) =
  boundOccurrences c term1 ++ boundOccurrences c term2


replace :: Id -> Id -> Term -> Term
replace old@(oldC, oldI) new@(newC, newI) term = case term of
  Const int ->
    Const int  -- Constants are unchanged

  Var (c, i) ->
    if (c, i) == old
    then Var new  -- Replace with new variable identifier
    else Var (c, i)  -- No change

  Lambda (c, i) body ->
    if (c, i) == old
    then Lambda (c, i) body  -- Do not replace inside the body as it's a new scope
    else Lambda (c, i) (replace old new body)  -- Recursively replace inside the body

  App term1 term2 ->
    App (replace old new term1) (replace old new term2)  -- Recursively replace in both subterms

  Plus term1 term2 ->
    Plus (replace old new term1) (replace old new term2)  -- Recursively replace in both subterms


-- Compute the set of free variables in a term
freeVars :: Term -> [Id]
freeVars (Const _) = []
freeVars (Var x) = [x]
freeVars (Lambda (c, i) t) = filter (/= (c, i)) (freeVars t)
freeVars (App t1 t2) = freeVars t1 ++ freeVars t2
freeVars (Plus t1 t2) = freeVars t1 ++ freeVars t2

-- Perform the substitution s [old -> new] in term
subst :: Id -> Term -> Term -> Term
subst x s t@(Const _) = t
subst x s t@(Var y) = if y == x then s else t
subst x s t@(Lambda y t1)
  | x == y = t
  | y `notElem` freeVars s = Lambda y (subst x s t1)
  | otherwise = let y' = (fst y, snd y + 1) -- Simple renaming strategy
                in Lambda y' (subst x s (replace y y' t1))
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2)


-- Check if a term is a value (which cannot be reduced further in CBV)
isValue :: Term -> Bool
isValue (Const _)    = True
isValue (Lambda _ _) = True
isValue _            = False

-- Call-by-value evaluation
cbv :: Term -> Term
cbv (Const n) = Const n
cbv (Var x)   = Var x

cbv (Lambda x t) = Lambda x t

cbv (App t1 t2) =
  let t1' = cbv t1
      t2' = cbv t2
  in case t1' of
      Lambda x tBody -> if isValue t2'
                        then cbv (subst x t2' tBody)
                        else App t1' t2'
      _ -> App t1' t2'

cbv (Plus t1 t2) =
  let t1' = cbv t1
      t2' = cbv t2
  in case (t1', t2') of
      (Const n1, Const n2) -> Const (n1 + n2)
      _ -> Plus t1' t2'

main :: IO ()
main = do
  -- putStrLn "Enter an integer:"
  -- input <- getLine
  -- case reads input of
  --   [(x, "")] -> putStrLn $ "You entered: " ++ show (x :: Int)
  --   _ -> putStrLn "Invalid input. Please enter an integer."

  -- let x = read input :: Integer

  -- time <- measureTime $ print ("fib = " ++ show (fibonacci x))
  -- putStrLn $ "Time taken: " ++ show (div (toNanoSecs time) (10 ^ 9)) ++ " s"

  -- let a = [1, 2, 3 :: Integer]
  -- let b = [4, 5, 6 :: Integer]
  -- print (mergeLists a b)
  -- print (mergeLists2 a b)

  -- let c = [1, 2, 3, 4 :: Integer]
  -- let d = [1, 2, 3, 4, 5 :: Integer]
  -- print (halve c)
  -- print (halve d)

  -- let e = [5, 4, 3, 2, 1 :: Integer]
  -- print (msort e)

  -- print (take 10 primes)

  -- print (iterate (+) 5)
  -- print "hello world"
  let o = freeOccurrences 'x' t1
  print o

  let b = boundOccurrences 'x' t2
  print b

  let oldId = ('x', 1)
  let newId = ('x', 3)
  let term = App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 1))))
  let replacedTerm = replace oldId newId term
  print replacedTerm

  let s = subst ('x', 0) (Var ('x', 1)) t1
  print s

