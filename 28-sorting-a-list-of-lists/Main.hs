module Main where

class LOrd a where
    cmp :: a -> a -> Int

instance LOrd [a] where
    cmp xs ys
        | length xs == length ys = 0
        | length xs < length ys  = 1
        | otherwise              = -1

merge :: (Show a, LOrd a) => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) []       = x:xs
merge [] (y:ys)       = y:ys
merge (x:xs) (y:ys)   =
    if cmp x y == 1 then [x] ++ merge xs (y:ys)
    else [y] ++ merge (x:xs) ys

lsort :: (Show a, LOrd a) => [a] -> [a]
lsort [] = []
lsort xs
    | length xs == 1 = xs
    | length xs == 2 = merge [xs !! 0] [xs !! 1]
    | otherwise = 
        case splitAt (((length xs) + 1) `div` 2) xs of
            (pref, suf) -> merge (lsort pref) (lsort suf)

main :: IO ()
main = putStrLn.show $ lsort ["abc","de","fgh","de","ijkl","mn","o"]
