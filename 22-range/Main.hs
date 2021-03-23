module Main where

range :: Int -> Int -> [Int]
range x y 
    | x > y     = error "x >= y"
    | x == y    = [y]
    | otherwise = [x] ++ range (succ x) y

main :: IO ()
main = putStrLn.show $ range 1 10