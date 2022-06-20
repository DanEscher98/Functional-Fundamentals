module Backtracking where

import           Control.Monad (join)
import           Prelude       hiding (return, (>>=))

-- Defining the Choice Monad
type Choice a = [a]
choose :: [a] -> Choice a
choose xs = xs

(>>=) :: Choice a -> (a -> Choice b) -> Choice b
choices >>= f = join (map f choices)

return :: a -> Choice a
return x = choose [x]

mzero :: Choice a
mzero = choose []

-- Either fail, or return something
-- useless and continue the computation.
guard :: Bool -> Choice ()
guard True  = return ()
guard False = mzero

-- ####################################

makePairs :: [a] -> [b] -> Choice (a, b)
makePairs xs ys = do
    x <- choose xs
    y <- choose ys
    return (x, y)

solveConstarint xs ys = do
    (x, y) <- makePairs xs ys
    guard (x*y == 8)
    return (x, y)

