module Main where

elementAt :: (Show a) => [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt [] n     = error "len(lst) < i+1"
elementAt (x:xs) n = elementAt xs (n-1)

main :: IO ()
main = sequence_ [
    putStrLn.show $ elementAt [1..10] 5,
    putStrLn.show $ elementAt [1] 2]