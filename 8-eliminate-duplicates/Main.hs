module Main where

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = 
    let curr = head xs in
        [curr] ++ compress (skipLike curr xs)
        where 
            skipLike :: (Eq a) => a -> [a] -> [a]
            skipLike _ [] = []
            skipLike a (x:xs)
                | a == x  = skipLike a xs
                | otherwise = (x:xs)

main :: IO ()
main = sequence_ [
    putStrLn.show.compress $ "aaaabccaadeeee"]