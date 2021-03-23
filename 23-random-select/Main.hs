module Main where

import System.Random

rndSelect :: (Show a) => [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect xs n =
    sequence $ replicate n getCh
    where 
        getCh = fmap (\g -> let idx = (head $ take 1 (randoms g :: [Int])) `mod` (length xs) in xs !! idx) newStdGen

main :: IO ()
main = rndSelect "abcdefgh" 3 >>= putStrLn