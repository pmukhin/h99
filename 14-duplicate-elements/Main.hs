module Main where

dupli :: (Show a) => [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

main :: IO ()
main = putStrLn.show.dupli $ [1, 2, 3]
