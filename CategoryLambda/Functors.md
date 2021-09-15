---
title: Functors and Applicatives
author: Daniel Sanchez
---

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
instances.

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

## The Functor Laws (covariant functor laws)
- The _identity law_: must transform the identity in the source 
    category to the identity in the destination category.
- The _compose law_: must transform the composition operator in
    the source category to the composition operator in the destination 
    category.

## The Application Laws
- 
