---
title: Monads introduction
author: Daniel Sanchez
---

## Definition
A monad is an algebraic structure in category theory. In Haskell it is 
used to describe computations as sequences of steps, and to handle side
effects such as state and IO. When dealing with values with context, 
Monad type-class helps us by automatically handling the context for us.

A monad consists of three objects, which must satisfy the _monad laws_.


## The three basic objects
- A type constructor **M**, such that for any type **a**, the type 
    **M a** is the type of a computation in the monad **M** that 
    produces a result of type **a**.
- The bind operator (`>>=`). A function that takes two computations
    and performs them one after the other, making the result of the
    first computation available to the second.
- The return function `return`. A function such that if **x::a**,
    then **return x** is a computation in **M** that, when executed
    will produce a value of type **a**.


## The Monad type class
```haskell
class Monad m where
return  :: a -> m a
(>>=)   :: m a -> (a -> m b) -> m b -- bind
(>>)    :: m a -> m b -> m b        -- then
-- m >> n = m >>= _ -> n
fail    :: String -> m a
```

## The Monad Laws
- The _right unit law_: `m >>= return` equates to `m`
- The _left unit law_: `return x >>= f` equates to `f x`
- The _associativity law_: `(m >>= f) >>= g` equates to 
    `m >>= (\x -> f x >>= g)`

## The Functor Laws (covariant functor laws)
- The _identity law_: must transform the identity in the source 
    category to the identity in the destination category.
- The _compose law_: must transform the composition operator in
    the source category to the composition operator in the destination 
    category.

## Syntax rules for `do`
```haskell
do { x }            --> x
do { x; <xs> }      --> x >> do { <xs> }
do { a <- x; <xs> } --> x >>= a -> do { <xs> }
do { let <declarations>; xs }
--> let <declarations> in do { xs }
```

## Implementation of `return`, `bind` and `then`
```haskell
return :: a -> IO a
return x = \i0 -> (x, i0)

(>>=)   :: IO a -> (a -> IO b) -> IO b
m >>= k = \i0 ->    let (x, i1) = m i0 in
                    let (y, i2) = k x i1 in 
                    (y, i2)
```

## Functors and Application

**Functors**: Uniform action over a parametrized type, generalizing the map
function on lists. A functor transforms one category into another category.
Functors arise every time we write compatibility layers and adapters between
different pieces of software. In Haskell, the Functor class only encompass 
the narrow case where the source and target categories are both categories
of ordinary functions

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) :: (a -> b) -> f a -> f b

-- map id       == id               identity
-- map (f . g)  == map f . map g    composition
```

**Applicative**: Map function in a context to the value in a context.
Can be chained together. All Applicative instances must also be Functor
instances. Given that, with Applicative we can apply function in a context 
to the value in a context.

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

When one uses a **monad transformer** it's possible appreciate that we are
using a **functor** as an adapter layer between two categories: the base
monad's Kleisli category and the transformed monad's Kleisli category.

The functor design pattern embodies a philosophy of programming that
emphasizes:
- compatibility over standardization
- specialization over monolithic frameworks
- short-term completion over future-proofing
