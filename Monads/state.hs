module StateMonad where

import           Control.Monad.State

fromStoAandS :: Int -> ([Int], Int)
fromStoAandS c
    | c `mod` 5 == 0 = ([c], c+1)
    | otherwise = ([c], c+1)

stateIntString :: State Int [Int]
stateIntString = state fromStoAandS

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
