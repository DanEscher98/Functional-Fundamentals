---
title: Functors and Applicatives
author: Daniel Sanchez
---

## Functors

Uniform action over a parametrized type, generalizing the map
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

It is possible generalize the `fmap` function.

```haskell
pure :: a -> f a
(<*>):: f (a -> b) -> f a -> f b

fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x

fmap2 :: (a -> b -> c) -> f a -> f b -> b c
fmap2 g x y = pure g <*> x <*> y
```

### The Functor Laws (covariant functor laws)
- The _identity law_: must transform the identity in the source 
    category to the identity in the destination category.
- The _compose law_: must transform the composition operator in
    the source category to the composition operator in the destination 
    category.

---

## Applicative

Map function in a context to the value in a context. The `<*>` operator
generalize the `fmap` function. Can be chained together. All Applicative
instances must also be Functor instances.

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

### The Application Laws
```haskell
pure <*> id x   == x
pure (g x)      == pure g <*> pure x
x <*> pure y    == pure (\g -> g y) <*> x
x <*> y <*> z   == (pure (.) <*> x <*> y) <*> z

-- Proof of 1st law
pure <*> id x
(\x -> (\y -> x)) id <*> x  -- by def of pure
(\y -> id) <*> x
\z -> (\y -> id) z (x z)    -- by def of <*>
\z -> id (x z)              -- using partial function application
\z -> x z

-- Prof of 2nd law
pure (g x)                  -- LEFT HAND SIDE
\y -> g x                   -- by def of pure
pure g <*> pure x           -- RIGHT HAND SIDE
\y -> g <*> \z -> x         -- by def of pure
\t -> (\y -> g) t ((\z -> x) t) -- by def of <*>
\t -> (\y -> g) t x
\t -> g x

-- Proof of 3rd law
x <*> pure y                -- LEFT HAND SIDE
x <*> \z -> y
\t -> x t ((\z -> y) t)     -- by def of <*>
\t -> x t y
pure (\g -> g y) <*> x      -- RIGHT HAND SIDE
\z -> (\g -> g y) <*> x
\t -> (\z -> (\g -> g y)) t (x t)-- by def of <*>
\t -> (\g -> g y) (x t)
\t -> x t y

-- Proof of 4th law
x <*> (y <*> z)             -- LEFT HAND SIDE
x <*> (\t -> y t (z t))
\g -> x g ((\t-> y t (z t)) g)
\g -> x g (y g (z g))
(pure (.) <*> x <*> y) <*> z-- RIGHT HAND SIDE
((\t -> (.) (x t)) <*> y) <*> z
(\g -> ((.) (x g)) (y g)) <*> z
\v -> x v ((y v) (z v))

``` 
