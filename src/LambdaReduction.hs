module LambdaReduction (Term, reduceOnce, substitute) where

-- Тип данных для представления лямбда-термов
data Term = Var String | Abs String Term | App Term Term deriving Show

-- Функция для выполнения одного шага редукции
reduceOnce :: Term -> Term
reduceOnce (App (Abs x t12) t2) = substitute t12 x t2
reduceOnce (App t1 t2) = App (reduceOnce t1) t2
reduceOnce t = t

-- Функция для подстановки переменной в терм
substitute :: Term -> String -> Term -> Term
substitute (Var y) x t = if x == y then t else Var y
substitute (Abs y t1) x t2 = if x == y then Abs y t1 else Abs y (substitute t1 x t2)
substitute (App t1 t2) x t = App (substitute t1 x t) (substitute t2 x t)

-- main :: IO ()
-- main = do
--     let term = App (Abs "x" (Var "x")) (Var "y")
--     putStrLn $ "Original term: " ++ show term
--     let reducedTerm = reduceOnce term
--     putStrLn $ "Reduced term: " ++ show reducedTerm