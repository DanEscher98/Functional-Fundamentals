module StateMonad where

import           Control.Monad.State


newtype Estado s x = Estado (s -> (x, s))

instance Functor (Estado s) where
    fmap f (Estado g) = Estado $ \s0 ->
        let (a, s1) = g s0
         in (f a, s1)

instance Applicative (Estado s) where
    pure x = Estado (\s -> (x, s))


instance Monad (Estado s) where
    mv >>= g = Estado (\st ->
        let (Estado ff) = mv
            (y, st') = ff st
            (Estado gg) = g y
         in gg st')
    return x = Estado (\st -> (x, st))

fromStoAandS :: Int -> ([Int], Int)
fromStoAandS c
  | c `mod` 5 == 0 = ([c], c+1)
  | otherwise = ([c], c+1)

stateIntString :: State Int [Int]
stateIntString = state fromStoAandS

counter :: Int -> [Int]
counter n = evalState cntState (1, []) where
    cntState = do
        (x, xs) <- get
        if x > n
           then return xs
           else do put (succ x, x:xs)
                   cntState

fibonacci :: Int -> Int
fibonacci n = evalState fibState (0, 1, n) where
    fibState :: State (Int, Int, Int) Int
    fibState = do
        (x1, x2, n) <- get
        if n == 0
           then return x1
           else do put (x2, x1 + x2, pred n)
                   fibState

-- fib n = flip evalState (0,1) $ do {
--                                   forM_ [0..(n-1)] $ \_ -> do {
--                                                               (a,b) <- get;
--     put (b,a+b);
--                                                               }
--   (a,b) <- get
--   return a

fac :: Int -> Int
fac n = flip evalState (1, 1) $ do
    forM_ [1..n] $ \_ -> do
        (n, a) <- get
        put (n + 1, a * n)
    (n, a) <- get
    return a

factorial :: Int -> Int
factorial n = evalState fact_state (n, 1) where
    fact_state = do
        (n, f) <- get
        case n of
          0 -> return f
          _ -> put (pred n, f*n) >> fact_state


type GCDState = (Int, Int)
gcd :: State GCDState Int
gcd = do
    (x, y) <- getState
    case compare x y of
      EQ -> return x
        LT -> do putState (y, x)
                 gcd
        GT -> do putState (y, x - y)
                 gcd
                     where
                         getState :: State GCDState GCDState
        getState = State (\(x, y) -> ((x, y), (x, y))
        putState :: GCDState -> State GCDState ()
        putState (x', y') = State (\_ -> ((), (x', y')))
