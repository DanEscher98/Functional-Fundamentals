module Main (main) where

-- BASIC CONCURRENCY

import System.IO
import Control.Concurrent (forkIO, myThreadId,
    MVar, newEmptyMVar, takeMVar, putMVar,
    Chan, newChan, readChan, writeChan,
    QSem, newQSem, waitQSem, signalQSem,
    QSemN, newQSemN, waitQSemN, signalQSemN)

getGreeting :: String -> IO String
getGreeting msg = do
    tid <- myThreadId
    let greeting = "Hello with " ++ msg ++ " from thread: " ++ show tid
    return $! greeting

threadHelloMVar :: MVar () -> Chan () -> IO ()
threadHelloMVar mutex endFlags = do
    -- Compute value (finished before getting mutex)
    greeting <- getGreeting "MVar and Channels"
    -- Get mutex (aquires lock for output resource)
    takeMVar mutex -- Fix the race condition
    putStrLn greeting -- Use the resoure
    -- Release mutex (another thread can take over)
    putMVar mutex ()
    -- Signal end of thread
    writeChan endFlags ()

threadHelloSem :: QSem -> QSemN -> IO ()
threadHelloSem mutex endFlags = do
    greeting <- getGreeting "Semaphores"
    waitQSem mutex
    putStrLn greeting
    signalQSem mutex
    signalQSemN endFlags 1


fn :: Int -> Int -> MVar Int -> IO ()
fn a b mVar = do
    putMVar mVar $! (a + b)


main :: IO ()
main = do
    mVar <- newEmptyMVar
    forkIO $ fn 1 2 mVar
    result <- takeMVar mVar
    print result

    -- Disable buffering on stdout
    hSetBuffering stdout NoBuffering

    -- MVar and Channels
    let n = 10
    mutex <- newEmptyMVar   -- Lock stdout resource
    endCFlags <- newChan  -- Ensure that all threads end
    mapM_ (const $ forkIO $ threadHelloMVar mutex endCFlags) [1..n]
    putMVar mutex () -- Initialize mutex
    -- Wait for threads
    mapM_ (\_ -> readChan endCFlags) [1..n]

    -- Semaphores
    -- Init mutex and FIFO for end flags
    mutexS <- newQSem 0
    endSFlags <- newQSemN 0
    -- Spawn threads (threads are waiting for mutex before printing)
    mapM_ (const $ forkIO $ threadHelloSem mutexS endSFlags) [1..n]
    -- Give mutex its values (threads start aquiring mutex here)
    signalQSem mutexS
    -- Read n end flags (blocks until all threads have sent
    -- their end signal)
    waitQSemN endSFlags n

    {-
        - MVars are mutable locations that can be shared
        - Actions on MVars are atomic 
        - Chanels are shared FIFO queues implemented with MVars
    -}
