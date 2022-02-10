module Main where

import           Control.Monad.Random

main :: IO ()
main = do
    gen <- getStdGen
    let values = evalRand diceSums gen
    print . take 10 $ values

-- Generate sums from rolling two dices
diceSums :: RandomGen g => Rand g [Int]
diceSums = do
    xs <- dieRolls
    ys <- dieRolls
    return $ zipWith (+) xs ys

-- Single die roll function
dieRolls :: RandomGen g => Rand g [Int]
dieRolls = getRandomRs (1, 6)
