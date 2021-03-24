module Main where

import System.Random

diffSelect :: Int -> Int -> IO [Int]
diffSelect n lim = 
    sequence $ replicate n (fmap (\g -> head $ take 1 $ randomRs (1,lim) g) newStdGen)

main :: IO ()
main = diffSelect 6 49 >>= putStrLn.show