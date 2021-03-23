module Main where

rotate :: (Show a) => [a] -> Int -> [a]
rotate [] _     = []
rotate xs sft
    | sft > 0   = (drop sft xs) ++ (take sft xs)
    | sft < 0   = (drop ((length xs) + sft) xs) ++ take ((length xs) + sft) xs
    | otherwise = xs    

main :: IO ()
main = 
    let list = ['a','b','c','d','e','f','g','h'] in
        (putStrLn.show $ rotate list 3) *>
            (putStrLn.show $ rotate list (-2))