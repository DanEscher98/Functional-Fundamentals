module StateMonad where

import           Control.Monad.State

fromStoAandS :: Int -> ([Int], Int)
fromStoAandS c
    | c `mod` 5 == 0 = ([c], c+1)
    | otherwise = ([c], c+1)

stateIntString :: State Int [Int]
stateIntString = state fromStoAandS
