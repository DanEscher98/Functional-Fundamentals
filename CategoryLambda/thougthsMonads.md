---
title: Thoughs about Monads
author: Daniel Sanchez
---

## Applications of Monads
- For Haskell so loved the `world -> (a, world)` that it gave us the
    `IO Monad`, that whosoever composes with it should not be impure,
    but free of side effects.
- They have three properties that make them useful:
    1. Modularity: They allow computations to be composed from simpler
       computations and separate the combination strategy from the
       actual computations being performed.
    2. Flexibility: They allow functional programs to be much more
       adaptable than equivalent programs written without monads. This
       is because the monad distills the computational strategy into a
       single place instead of requiring it be distributed throughout
       the entire program.
    3. Isolation: They can be used to create imperative-style
       computational structures which remain safely isolated from the
       main body of the functional program. This is useful for
       incorporating side-effects which remain safely isolated from
       the main body of the functional program. This is useful for
       incorporating side-effects and state into a pure functional
       language.
- Mathematically all monads are functors.
- Monads are programmable semicolons.
- Monads are like *burritos*. A functor `F` takes each type `T` and
    maps it to a new type `F(T)`. A burrito takes a type, meat or
    beans, and turns it into a new type, like beef burrito.
    - functor: burrito
    - map: you can add onion to beans, so also to a bean burrito
    - pure: the tortilla which lifts any type to a burrito.
    - join: if I had burrito burrito, I can unwrap the inner burrito
- Think of monads as _statically typed filters_ in the Unix sense of
    "pipes and filters".
- In Haskell, `main` is `main :: IO ()` or `main :: () -> IO ()`. So, a
    Haskell program is just one big Kleisli arrow in the `IO` monad.
- In game programming, when a computer plays against a human, it can't
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
- If the program is purely functional, we can mathematically combine
    smaller programs to make complex ones and reason about the
    soundness and correctness of a program.
- A _Monad_ is a new type that the language knows how to treat when
    composing functions. They can encapsulate side effects or just
    purely functional computations and making function composition
    possible.
- Generally, Category Theory helps Computer Science by discovering
    “computational patterns”. Category Theory discovers them and studies
    them in order to find their mathematical properties. Then, Computer
    Science can make use of this knowledge in order to empower
    developers to create software in a more concise and more correct
    way.
- `Type ~> Functor ~> Applicative Functor ~> Monad`
    - `fmap :: (a -> b) -> F a -> F b` lifts a function. _If you will
        gave me a blueberry for each apple I gave you_ `(a -> b)` _,
        and I have a box of apples_ `(F a)` _, then I can get a box of
        blueberries_ `(F b)`.
    - `pure :: a -> M a` lifts a type to the _M_ realm, it's a NT.
        _If I have an apple_ `(a)` _then I can put it in a box_
        `(M a)`.
    - `(<*>) :: M (a -> b) -> M a -> M b` applies a function
    - `join :: M (M a) -> M a` _If I have a box of apples_ `M (M a)`
        _then I can take the apples from each, and put them in a new
        box_ `(M a)`.
    - `(>>=) :: M a -> (a -> M b) -> M b` provides a way to compose
        functions. It's a Natural Transformation (NT). _If I have a 
        box of apples_ `(M a)` _and for each apple you will give me a
        box of blueberries_ `(a -> M b)` _then I can get a box with
        all the blueberries together_ `(M b)`.
        ```haskell
        xs >>= f = join (fmap f xs)
        ```

## Code Examples

### List Monad
```haskell
instance Monad [] where
    join     = concat
    return x = [x]
    as >>= k = concat (fmap k as)

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f xs = xs >>= (return . f)
liftM f xs = do x <- xs
                return (f x)
```

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
get :: State s s
put :: s -> State s ()

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

## REFERENCES

- [A monad is just a ...](https://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem)
- [The correspondence between Monads in CT and Haskell](https://dkalemis.wordpress.com/2013/11/23/the-correspondence-between-monads-in-category-theory-and-monads-in-haskell/)
- [Monads as containers](https://wiki.haskell.org/Monads_as_containers)
- [Monads as computations](https://wiki.haskell.org/Monads_as_computation)
