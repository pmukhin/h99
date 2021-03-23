module Main where

myButLast :: (Show a) => [a] -> a

myButLast (x:y:[]) = x
myButLast (x:y:xs) = myButLast (y:xs)
myButLast xs       = error "len(lst) < 2" 

main :: IO ()
main = sequence_ [
    putStrLn . show . myButLast $ take 10 [1..],
    putStrLn . show $ myButLast [1]]