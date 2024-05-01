fibonacci :: Integer -> Integer
fibonacci 1 = 0
fibonacci 2 = 1
fibonacci 3 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

mergeLists :: [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists (x:xs) ys = x : mergeLists xs ys

mergeLists2 :: [a] -> [a] -> [a]
mergeLists2 xs ys = foldr (:) ys xs

halve :: [a] -> [([a], [a])]
halve a = do
    let n = length a
    let half = div n 2
    if mod n 2 == 1
        then [splitAt (half + 1) a]
        else [splitAt half a]

main :: IO ()
main = do
    let a = [1, 2, 3]
    let b = [4, 5, 6]
    print (mergeLists a b)
    print (mergeLists2 a b)
    print (fibonacci 5)
    let c = [1,2,3,4]
    let d = [1,2,3,4,5]
    print (halve c)
    print (halve d)