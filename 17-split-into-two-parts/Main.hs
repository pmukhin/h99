module Main where

split :: (Show a) => [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split xs n =
    ((take n xs), (drop n xs))

main :: IO ()
main = (putStrLn.show $ split "abcdefghik" 3) *> 
    (putStrLn.show $ split "abc" 3) *>
    (putStrLn.show $ split "ab" 3)