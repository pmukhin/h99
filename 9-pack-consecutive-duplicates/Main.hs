module Main where

pack :: (Eq a, Show a) => [a] -> [[a]]
pack [] = []
pack xs = [replicate currLen curr] ++ (pack $ drop currLen xs)
    where
        curr = head xs 
        currLen = cnt curr xs
        cnt :: (Eq a) => a -> [a] -> Int
        cnt _ [] = 0
        cnt curr (x:xs)
            | curr == x = 1 + cnt curr xs
            | otherwise = 0

main :: IO ()
main = putStrLn.show.pack $ "aaaabccaadeeee"