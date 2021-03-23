module Main where

myLength :: [a] -> Int
myLength = cnt 0 where
    cnt :: Int -> [a] -> Int 
    cnt acc []     = acc
    cnt acc (x:xs) = cnt (succ acc) xs

main :: IO ()
main = sequence_ [
    putStrLn.show $ myLength [123, 456, 789],
    putStrLn.show $ myLength "Hello, world!"]