module Main where

slice :: (Show a) => [a] -> Int -> Int -> [a]
slice xs s e =
  take (pred e) (drop (pred s) xs)
 
main :: IO ()
main = putStrLn.show $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7