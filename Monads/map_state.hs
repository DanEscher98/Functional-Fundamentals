module MapState where

import           Control.Monad.State (State, evalState, get, modify, put)

addAnIncrementingAmount :: [Int] -> [Int]
addAnIncrementingAmount numbers =
    reverse . fst $ foldl addAmount ([], 0) numbers

addAmount :: ([Int], Int) -> Int -> ([Int], Int)
addAmount (result, amountToAdd) number =
    (number + amountToAdd : result, succ amountToAdd)

-----------------------------------------------------------
-- Generalising with a new higher order function ----------
addAnIncrementingAmount' :: [Int] -> [Int]
addAnIncrementingAmount' = mapWithState (addAmount', 0)

-- map :: (a -> b) -> [a] -> [b]
-- foldl :: (b -> a -> b) -> b -> t a -> b
-- Here, the function that map take, changes
mapWithState :: (a -> state -> (b, state), state) -> [a] -> [b]
mapWithState _ [] = []
mapWithState (f, state) (x:xs) = x' : mapWithState (f, state') xs
    where (x', state') = f x state

addAmount' :: Int -> Int -> (Int, Int)
addAmount' n state = (n+state, succ state)

myfunk1 :: Int -> Int -> (Int, Int)
myfunk1 n state
    | mod n 2 == 0 = (n * state, pred state)
    | otherwise = (div n state, succ state)

reversa :: [a] -> [a]
reversa = foldl (flip (:)) []

-----------------------------------------------------------
-- The State Monad ----------------------------------------

performCalculation :: Int -> Int
performCalculation = evalState statefulCalculation

statefulCalculation :: State Int Int
statefulCalculation = do
    initialValue <- get
    modify (+3)
    modify (*2)
    newValue <- get
    return (newValue - initialValue)

    {-  def perform_calculation(init_val):
            new_value = initial_val + 3
            new_value = new_value * 2
            return (new_value - init_val)-}

addAmount'' :: Int -> State Int Int
addAmount'' number = do
    amountToAdd <- get
    modify succ
    return (number + amountToAdd)

-- mapM ~ sequence . map
addAnIncrementingAmount'' :: [Int] -> [Int]
addAnIncrementingAmount'' ns =
    evalState (mapM addAmount'' $ ns) 0

factorial :: Int -> [Int]
factorial n = evalState (mapM fac [1..n]) 1 where
    fac :: Int -> State Int Int
    fac n = do
        modify (*n)
        n' <- get
        return n'

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = evalState (fac n) (1, 1) where
    fac :: Int -> State (Int, Int) Int
    fac n = do
        (i, cache) <- get
        if i == n
           then return (i*cache)
           else do
               put (succ i, i*cache)
               fac n

fibonacci :: Int -> Int
fibonacci n = evalState (fib n) (0, 1) where
    fib :: Int -> State (Int, Int) Int
    fib n = do
        (a, b) <- get
        if n == 0
           then return a
           else do
               put (b, a+b)
               fib (pred n)

-- REFERENCES:
--      https://tomphp.github.io/blog/haskell-mapping-with-state
--      https://stackoverflow.com/questions/54117352/unable-to-understand-how-state-monad-get-its-state-in-this-code
--      https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
