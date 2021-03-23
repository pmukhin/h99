module Main where

encode :: (Eq a, Show a) => [a] -> [(Int, a)]
encode [] = []
encode xs = [(currLen, curr)] ++ (encode $ drop currLen xs)
    where
        curr = head xs 
        currLen = cnt curr xs
        cnt :: (Eq a) => a -> [a] -> Int
        cnt _ [] = 0
        cnt curr (x:xs)
            | curr == x = 1 + cnt curr xs
            | otherwise = 0


main :: IO ()
main = putStrLn.show.encode $ "aaaabccaadeeee"