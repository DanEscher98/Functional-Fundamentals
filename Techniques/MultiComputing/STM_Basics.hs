module Main (main) where

import System.IO
import Control.Concurrent
import Control.Concurrent.STM

-- Number of finished transactions
type Result = TVar (Int, Int)

addToResult :: Result -> Int -> STM ()
addToResult result x = do
    modifyTVar result (\(sum, endCtr) -> (sum+x, succ endCtr))

-- Waits for a number of completed transactions, then
-- returns the sum result
waitForCounter :: Result -> Int -> STM Int
waitForCounter result limit = do
    (sum, endCtr) <- readTVar result
    -- The retry pesimistically checks for synchronization
    if endCtr < limit then retry else return sum


main :: IO ()
main = do
    -- Number of threads to be spawned
    let n = 100
    -- Set up TVar
    result <- newTVarIO (0, 0)
    -- Spawn threads
    mapM_ (forkIO . atomically . addToResult result) [1..n]
    -- Wait for threads and get sum
    sum <- atomically $ waitForCounter result n
    putStrLn $ "Sum of [1..n] = " ++ show sum
    

    {-
        - Goal: Atomicity of transactions of a shared resource without locks
        - Transactional Memory:
            - critical sections: atomic transactions
            - if a commit arises on commit: rerun
        - No deadlocks are possible
    -}
