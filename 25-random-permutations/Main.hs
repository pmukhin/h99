module Main where

import System.Random

rndPermu :: (Show a) => [a] -> IO [a]
rndPermu [] = return []
rndPermu xs = do
    idx  <- fmap (\std -> head $ take 1 $ randomRs (0, pred $ length xs) std) newStdGen
    rest <- rndPermu ((take idx xs) ++ drop (succ idx) xs)
    let res = [(xs !! idx)] ++ rest
    return res

main :: IO ()
main = rndPermu "abcdef" >>= putStrLn