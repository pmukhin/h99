module Main where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs = 
    if (length xs) `mod` 2 == 1 then
        cmp (take n xs) (drop (succ n) xs)
    else 
        cmp (take n xs) (drop n xs)
    where
        n = (length xs) `div` 2
        cmp :: (Eq a) => [a] -> [a] -> Bool
        cmp [] (y:ys) = False
        cmp (x:xs) [] = False
        cmp [] []     = True
        cmp (x:xs) ys = 
            x == (last ys) && cmp xs (take (length ys - 1) ys)

main :: IO ()
main = 
    sequence_ [
        putStrLn.show.isPalindrome $ [1,2,3,4,3,2,1], -- first type
        putStrLn.show.isPalindrome $ [1,2,3,2,1], -- snd type
        putStrLn.show.isPalindrome $ [1],
        putStrLn.show.isPalindrome $ [1,2,3],
        putStrLn.show.isPalindrome $ [1,2]]