module Sorting where

qsort :: Ord a => [a] -> [a]
qsort []        = []
qsort (x:xs)    = qsort smaller ++ [x] ++ qsort bigger
        where smaller   = [a | a <- xs, a <= x]
              bigger    = [b | b <- xs, b >  x]

isSorted :: Ord a => [a] -> Bool
isSorted []         = True
isSorted [_]        = True
isSorted (x:xs)
    | x > head xs   = False
    | otherwise     = isSorted xs
