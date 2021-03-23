module Main where

dropEvery :: (Show a) => [a] -> Int -> [a]

dropEvery [] _ = []
dropEvery xs 0 = xs
dropEvery xs 1 = []
dropEvery xs n = drop xs n where
    drop :: [a] -> Int -> [a]
    drop [] _ = []
    drop (x:xs) 0 = drop xs n
    drop (x:xs) n = [x] ++ drop xs (pred n) 

main :: IO ()
main = putStrLn $ dropEvery "abcdefghik" 3