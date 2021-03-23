module Main where

data NestedList a = Elem a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]

myFlatten (Elem a)    = [a]
myFlatten (List [])   = []
myFlatten (List xs)   = 
    (unpack xs) where
        unpack :: [NestedList a] -> [a]
        unpack [] = []
        unpack ((Elem a):ys) =
            [a] ++ (unpack ys)
        unpack ((List (x:xs)):ys) = 
            (myFlatten x) ++ (unpack xs) ++ (unpack ys)

main :: IO ()
main = 
    putStrLn.show.myFlatten $ List [
        Elem 0, List [Elem 1, List [Elem 2, Elem 3, Elem 4, Elem 5], Elem 6]]