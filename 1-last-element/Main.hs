module Main where

myLast :: (Show a) => [a] -> a

myLast []     = error "list is empty"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

main :: IO ()
main = putStrLn . show . myLast $ take 10 [1..]