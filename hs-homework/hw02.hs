import System.Clock
import Text.Read (readMaybe)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists l1@(x : xs) l2@(y : ys)
    | x < y = x : mergeLists xs l2
    | otherwise = y : mergeLists l1 ys 

mergeLists2 :: [a] -> [a] -> [a]
mergeLists2 xs ys = foldr (:) ys xs

halve :: [a] -> ([a], [a])
halve a
    | mod n 2 == 1 = splitAt (half + 1) a
    | otherwise = splitAt half a
    where
        n = length a
        half = div n 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs =
    let (l1, l2) = halve xs in
        mergeLists (msort l1) (msort l2)

-- Measure the time of function execution
measureTime :: IO () -> IO TimeSpec
measureTime action = do
    start <- getTime Monotonic
    action
    end <- getTime Monotonic
    return $ diffTimeSpec end start

main :: IO ()
main = do

    putStrLn "Enter an integer:"
    input <- getLine
    case reads input of
        [(x, "")] -> putStrLn $ "You entered: " ++ show (x :: Int)
        _         -> putStrLn "Invalid input. Please enter an integer."

    let x = read input :: Integer

    time <- measureTime $ print ("fib = " ++ show (fibonacci x))
    putStrLn $ "Time taken: " ++ show (div (toNanoSecs time) (10 ^ 9)) ++ " s"
    
    let a = [1, 2, 3]
    let b = [4, 5, 6]
    print (mergeLists a b)
    print (mergeLists2 a b)

    let c = [1,2,3,4]
    let d = [1,2,3,4,5]
    print (halve c)
    print (halve d)

    let e = [5,4,3,2,1]
    print (msort e)