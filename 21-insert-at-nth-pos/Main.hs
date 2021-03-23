module Main where

insertAt :: (Show a) => a -> [a] -> Int -> [a]
insertAt x xs i
    | i < 1         = error "i < 1"
    | i > length xs = error "len(xs) < i" 
    | otherwise     = (take (pred i) xs) ++ [x] ++ drop (pred i) xs

main :: IO ()
main = putStrLn $ insertAt 'X' "abcd" 2