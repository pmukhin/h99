module Main where

repli :: (Show a) => [a] -> Int -> [a]
repli xs 0 = []
repli xs n = xs ++ repli xs (pred n)

main :: IO ()
main = putStrLn.show $ repli "abc" 3