module Main (main) where

import Control.Parallel.Strategies

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

main :: IO ()
main = do
    print $! runEval $ do
        a <- rpar $ fib 32
        b <- rpar $ fib 12
        rseq a
        rseq b
        return (a, b)
