module Main where

import System.Random

rndSelect :: (Show a) => [a] -> Int -> IO [a]
rndSelect _ 0 = return []
rndSelect xs n =
    sequence $ replicate n getCh
    where 
        getRnd g = (head $ take 1 (randoms g :: [Int]))
        getCh = fmap (\g -> let idx = (getRnd g) `mod` (length xs) in xs !! idx) newStdGen

main :: IO ()
main = rndSelect "abcdefgh" 3 >>= putStrLn