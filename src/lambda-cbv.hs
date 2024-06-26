import Data.List

-- Задание: реализовать интерпретатор с подстановками и вызовом по
-- значению (ДЛ, раздел 2.4.2, упражнение 2.15(2)).

-- Рассмотрим следующий язык.

data Term =
    Const Int
  | Var Id
  | Lambda Id Term
  | App Term Term
  | Plus Term Term
    deriving Show

-- Пример: λx1. x1 (λx2. x0 x2)

t1 :: Term
t1 = (Lambda ('x', 1)
      (App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 2))))))

-- Пример: λx1. x1 (λx2. x0 x1)

t2 :: Term
t2 = (Lambda ('x', 1)
      (App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 1))))))

-- В данном разделе рассматривается интерпретатор с подстановнками, а
-- не окружениями. Поэтому сначала следует реализовать функцию subst t
-- x s, которая возвращает результат подстановки терма s вместо
-- переменной x в терм t. В лекции 3, слайд 12 результат подстановки
-- обозначался через t[s/x], а в книге Пирса, с. 74, определение 5.3.5 —
-- через [x |-> s]t.

-- При реализации указанных определений возникает сложность с выбором
-- переменной, которая при необходимости используется для
-- переименования связанной переменной. Условимся, что все имена
-- переменных состоят из одного символа, за которым следует
-- неотрицательное целое число. Для этого можно объявить следующий
-- тип.

type Id = (Char, Int)

-- Рассмотрим подстановку (λy. N)[M/x], когда y ∈ FV(M). В этом случае
-- нужно выбрать переменную y', которая не входит в M, и переименовать
-- y в y'. Для этого предлагается написать следующие функции.

-- freeOccurrences с t находит свободные вхождения переменных (с, n),
-- которые содержат указанный символ c и различные индексы n в терме t,
-- и возвращает список таких индексов.

freeOccurrences :: Char -> Term -> [Int]
freeOccurrences = undefined

-- > freeOccurrences 'x' t1
-- [0]

-- boundOccurrences с t находит связанные вхождения переменных (с, n),
-- которые содержат указанный символ c и различные индексы n в терме t,
-- и возвращает список таких индексов.

boundOccurrences :: Char -> Term -> [Int]
boundOccurrences = undefined

-- > boundOccurrences 'x' t1
-- [1,2]

-- replace x y t возвращает результат замены свободных вхождений
-- переменной x на переменную y в терме t согласно следующим правилам.
-- Предполагается, что y не имеет связанных вхождений в t. Обозначим
-- replace x y t через (x -> y)t.

-- (x -> y)n = n для константы n
-- (x -> y)x = y
-- (x -> y)z = z, если x ≠ z
-- (x -> y)(s t) = (x -> y)s (x -> y)t
-- (x -> y)(s + t) = (x -> y)s + (x -> y)t
-- (x -> y)(λx. t) = λx. t
-- (x -> y)(λz. t) = λz. (x -> y)t, x ≠ z.

-- Таким образом, единственная разница между (x -> y)t и обычной
-- подстановкой t[y/x] заключается в отсутствии проверки z ∈ FV(y),
-- то есть z = y, в правиле для (x -> y)(λz. t). Здесь используется
-- предположение об отсутствии связанных вхождений y в терме, в котором
-- делается замена.

replace :: Id -> Id -> Term -> Term
replace = undefined

-- > replace ('x', 1) ('x', 3) (App (Var ('x', 1)) (Lambda ('x', 2) (App (Var ('x', 0)) (Var ('x', 1)))))
-- App (Var ('x',3)) (Lambda ('x',2) (App (Var ('x',0)) (Var ('x',3))))

-- subst x s t возвращает подстановку s вместо x в t.

-- n[s/x] = n для константы n
-- x[s/x] = s
-- z[s/x] = z, если x ≠ z
-- (t1 t2)[s/x] = t1[s/x] t2[s/x]
-- (t1 + t2) = t1[s/x] + t2[s/x]
-- (λx. t)[s/x] = λx. t
-- (λy. t)[s/x] = λy. t[s/x], если x ≠ y и y ∉ FV(s)
-- (λy. t)[s/x] = λy'. ((y -> y')t)[s/x], если x ≠ y и y ∈ FV(s)

-- В предыдущей строке y' есть переменная с тем же символом c, что и y.
-- Индекс этой переменной определяется следующим образом. Пусть fs
-- есть список индексов свободных переменных с символом c в терме s, и
-- пусть bs есть список индексов связанных переменных с символом c в
-- терме t. Индекс y' равен 1 плюс максимум элементов fs и bs.
-- Таким образом, y' отлична от всех свободных переменных терма s
-- и от всех связанных переменных терма t. Это делает возможность
-- замены (y -> y')t с помощью функции replace.

subst :: Id -> Term -> Term -> Term
subst = undefined

-- Напоминание: t1 = λx1. x1 (λx2. x0 x2), t2 = λx1. x1 (λx2. x0 x1)

-- t1[x1/x0] = λx3. x3 (λx2. x1 x2)
-- subst ('x', 0) (Var ('x', 1)) t1

-- t2[x1/x0] = λx3. x3 (λx2. x1 x3)
-- subst ('x', 0) (Var ('x', 1)) t2

-- t2[y1/x0] = λx1. x1 (λx2. y1 x1)
-- subst ('x', 0) (Var ('y', 1)) t2

-- Интерпретатор. См. правила в ДЛ, раздел 2.4.2.

cbv :: Term -> Term
cbv = undefined

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
 
-- > checkOp plusChurch 3 5
-- 8
-- > checkOp timesChurch 3 5
-- 15
