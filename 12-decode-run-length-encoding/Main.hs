module Main where

data RunLength a = Single a | Multiple Int a
    deriving Show

encode :: (Eq a, Show a) => [RunLength a] -> [a]
encode [] = []
encode ((Single a):xs) = [a] ++ encode xs
encode ((Multiple n a):xs) = (replicate n a) ++ encode xs

main :: IO ()
main = putStrLn.show.encode $ [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']