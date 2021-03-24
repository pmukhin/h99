module Main where

import System.Random

rndPermu :: (Show a) => [a] -> Int -> IO [a]
rndPermu [] n     = return []
rndPermu _ 0      = return []
rndPermu (x:[]) n = return [x]
rndPermu xs n     = do
    idx  <- fmap (\std -> head $ take 1 $ randomRs (0, pred $ length xs) std) newStdGen
    rest <- rndPermu ((take idx xs) ++ drop (succ idx) xs) (pred n)
    let res = [(xs !! idx)] ++ rest
    return res

combinations :: (Show a) => Int -> [a] -> IO [[a]]
combinations n xs = 
    sequence $ replicate n (rndPermu xs n)

main :: IO ()
main = (fmap show (combinations 3 "abcdef")) >>= putStrLn