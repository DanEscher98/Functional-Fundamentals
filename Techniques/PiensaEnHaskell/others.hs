module Others where

import           Data.Char (ord)

len :: [a] -> Int
len []     = 0
len (x:xs) = succ . len $ xs

range :: Int -> [Int]
range n = loop 0 n where
    loop _ 0 = []
    loop i n = i : loop (succ i) (pred n)

apply :: Int -> (a -> a) -> (a -> a)
apply 0 _ = id
apply n f = f . (apply (pred n) f)
