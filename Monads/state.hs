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

<<<<<<< HEAD
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
=======
reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount funcCount ls =
    (succ funcCount, reverse ls)

appendReverseWithCount :: Int -> [a] -> [a] -> (Int, [a])
appendReverseWithCount funcCount ls1 ls2 =
    let (funcCount', revLs1) = reverseWithCount funcCount ls1
        (funcCount'', revLs2) = reverseWithCount funcCount' ls2
     in (succ funcCount'', revLs1 ++ revLs2)

-- aStatefulFunction :: state -> (state, a)
data Estado s a = Estado { ejecutaEstado :: s -> (s, a) }

instance Functor (Estado s) where
    fmap :: (a -> b) -> Estado s a -> Estado s b
    fmap f (Estado stateFn) = Estado (\s ->
        let (s', result) = stateFn s
         in (s', f result))

instance Applicative (Estado s) where
    pure :: a -> Estado s a
    pure x = Estado (\s -> (s, x))
    (<*>) :: Estado s (a -> b) -> Estado s a -> Estado s b
    (<*>) (Estado stateFx) (Estado stateX) = Estado (\s ->
        let (s', fx) = stateFx s
            (s'', x) = stateX s'
         in (s'', fx x))

instance Monad (Estado s) where
    return = pure
    (>>=) :: Estado s a -> (a -> Estado s b) -> Estado s b
    (>>=) (Estado stateX) nextFn = Estado (\s ->
        let (s', x) = stateX s
         in ejecutaEstado (nextFn x) s')

obtener :: Estado s s
obtener = Estado (\s -> (s, s))

asignar :: s -> Estado s ()
asignar s = Estado (\_ -> (s, ()))

modificar :: (s -> s) -> Estado s ()
modificar f = do
    s <- obtener
    asignar (f s)

evaluaEstado :: Estado s a -> s -> a
evaluaEstado state = snd . ejecutaEstado state

factorial :: Int -> Int
factorial n = evaluaEstado (fac n) 1 where
    fac n = do
        cache <- obtener
        if n == 0
           then return cache
           else do
                modificar (*n)
                fac (pred n)

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = (*n) . factorial' . pred $ n



-- REFERENCES
-- https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
-- http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
>>>>>>> d28086a660047efa63451997646a64282393a765
