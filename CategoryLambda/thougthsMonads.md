---
title: Thoughs about Monads
author: Daniel Sanchez
---

## Applications of Monads
- Monads are programmable semicolons.
- *Haskell so loved the `world -> (a, world)` that gave us the monad.*
- In Haskell, `main` is `main :: IO ()` or `main :: () -> IO ()`. So, a
    Haskell program is just one big Kleisli arrow in the `IO` monad.
- In game programming, when a computer plays against a human, it can’t
    predict the opponent’s next move. It can, however, generate a list
    of all possible moves and analyze them one by one.
- A function that has has read-only access to some external state, or 
    environment, can be always replaced by a function that takes that
    environment as an additional argument `(a, e) -> b`.
- When is called, The function `getChar :: () -> IO Char` returns a 
    character inside a container, and each time it would return exactly
    the same container. Conceptually, this container would contain the
    superposition of all possible characters. It's possible compose
    `getChar` with another *Kleisli* arrow, but this second arrow could
    only return his value as an `IO a` (a supperposition of all possible
    `a` values). *There is no* `runIO`.


## Code Examples

### List Monad
```haskell
instance Monad [] where
    join     = concat
    return x = [x]
    as >>= k = concat (fmap k as)
```

### Maybe Monad
```haskell
instance Monad Maybe where
    Nothing >>= k = Nothing
    Just a  >>= k = k a
    return a      = Just a
```

### State Monad
```haskell
newtype State s a = State (s -> (a, s))
runState :: State s a -> s -> (a, s)
runState (State f) s = f s

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s' = State (\s -> ((), s'))

instance Monad (State s) where
    sa >>= k = State (\s -> let (a, s') = runState sa s
                            in runState (k a) s')
    return a = State (\s -> (a, s))
```

### Reader Monad
```haskell
newtype Reader e a = Reader (e -> a)

instance Monad (Reader e) where
    ra >>= k = Reader (\e -> runReader (k (runReader ra e)) e)
    return x = Reader (\e -> x)

runReader :: Reader e a -> e -> a
runReader (Reader f) e = f e
```

### Continuations *Don't call us, we'll call you!*
```haskell
data Cont r a = Cont ((a -> r) -> r)

runCont :: Cont r a -> (a -> r) -> r
runCont (Cont k) h = k h

-- Our goal is create a function that takes
-- the handler `(b -> r)` and produces the result `r`.
-- (>>=) :: ((a -> r) -> r) ->
--          (a -> (b -> r) -> r) ->
--          ((b -> r) -> r)
instance Monad (Cont r) where
    ka >>= kab = Cont (\hb -> runCont ka (\a -> runCont (kab a) hb))
    return a = Cont (\ha -> ha a)
```

