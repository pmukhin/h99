module Main where

reverse' :: (Show a) => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

main :: IO ()
main = putStrLn.show.reverse' $ [1..10]