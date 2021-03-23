module Main where

data RunLength a = Single a | Multiple Int a
    deriving Show

encode :: (Eq a, Show a) => [a] -> [RunLength a]
encode [] = []
encode xs = [bld curr currLen] ++ (encode $ drop currLen xs)
    where
        curr = head xs 
        currLen = cnt curr xs
        cnt :: (Eq a) => a -> [a] -> Int
        cnt _ [] = 0
        cnt curr (x:xs)
            | curr == x = 1 + cnt curr xs
            | otherwise = 0
        bld :: a -> Int -> RunLength a
        bld a 1 = Single a
        bld a n = Multiple n a 

main :: IO ()
main = putStrLn.show.encode $ "aaaabccaadeeee"