module Miscellaneous where

ackerman :: Int -> Int -> Int
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)
