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

-- \s\z. z
churchZero :: Term
churchZero = Lambda ('s', 0) (Lambda ('z', 0) (Var ('z', 0)))

-- plus = λm. λn. λs. λz. m s (n s z)
plusChurch :: Term
plusChurch =
  Lambda ('m', 0) (Lambda ('n', 0) (Lambda ('s', 0) (Lambda ('z', 0)
    (App (App (Var ('m', 0)) (Var ('s', 0)))
      (App (App (Var ('n', 0)) (Var ('s', 0))) (Var ('z', 0)))))))

-- times = λ m. λ n. m (plus n) 0
timesChurch :: Term
timesChurch =
  Lambda ('m', 0) (Lambda ('n', 0)
    (App (App (Var ('m', 0)) (App plusChurch (Var ('n', 0)))) churchZero))

intToChurch :: Int -> Term
intToChurch n =
  Lambda ('s', 0) (Lambda ('z', 0) $ iterate (App (Var ('s', 0))) (Var ('z', 0)) !! n)
 
-- Пример: intToChurch 3 = \s\z. s (s (s z))
 
-- Чтобы перевести терм \s\z. s^n z в n, применяем его к функции
-- прибавления единицы и начальному значению 0, затем вычисляем
-- значение полученного терма.

churchToInt :: Term -> Int
churchToInt t =
  let Const n = cbv (App (App t (Lambda ('x', 0) (Plus (Var ('x', 0)) (Const 1)))) (Const 0)) in n
 
checkOp :: Term -> Int -> Int -> Int
checkOp t m n = churchToInt $ App (App t (intToChurch n)) (intToChurch m)

churchOne :: Term
churchOne = Lambda ('s', 1) 
              (Lambda ('z', 2) 
                (App (Var ('s', 1)) (Var ('z', 2))))

churchTwo :: Term
churchTwo = Lambda ('s', 1) 
              (Lambda ('z', 2) 
                (App (Var ('s', 1)) 
                  (App (Var ('s', 1)) (Var ('z', 2)))))

churchThree :: Term
churchThree = Lambda ('s', 1) 
               (Lambda ('z', 2) 
                 (App (Var ('s', 1)) 
                   (App (Var ('s', 1)) 
                     (App (Var ('s', 1)) (Var ('z', 2))))))

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

  putStr "\ns subst\n"
  let s = subst ('x', 0) (Var ('x', 1)) t1
  print s

  putStr "\n"
  putStr ("Church's one: \n")
  print (churchOne)

  putStr "\n"
  putStr "Church's two:\n"
  print (churchTwo)

  putStr "\n"
  putStr "Church's three:\n"
  print (churchThree)

  putStr "\n"
  print plusChurch

  putStr "\n"
  let subts1 = (subst ('m', 0) churchOne plusChurch)
  print subts1

  putStr "\n"
  let subts2 = (subst ('n', 0) churchTwo subts1)
  print subts2

  print (cbv subts2)

  print (churchToInt churchThree)