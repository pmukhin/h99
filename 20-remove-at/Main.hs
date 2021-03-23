module Main where

removeAt :: (Show a) => Int -> [a] -> (a, [a])
removeAt n xs 
    | n < 1            = error "n < 1"
    | n > (length xs)  = error "len(xs) <= n"
    | otherwise        = (xs !! (pred n), (take (pred n) xs) ++ drop n xs)

main :: IO ()
main = (putStrLn.show $ removeAt 2 "abcd") *>
    (putStrLn.show $ removeAt 4 "abcd")