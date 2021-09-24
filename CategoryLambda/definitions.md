---
title: Definitions of Abstract Algebra
author: Daniel Sanchez
---

## Algebras
- An algebra is defined by:
    1. A set of *values*
    2. A set of *operations*
    3. A set of *laws*
- A *semigroup*, *monoid* or *group* is a kind of algebra with
    a defined binary operator $\ast$

| if $\ast$     | Semigroup | Monoid    | Group     | Abelian | 
|:-----         | :-----:   | :-----:   | :-----:   | :-----: | 
|is closed      |   **T**   |   **T**   |   **T**   |   **T** | 
|is associative |   **T**   |   **T**   |   **T**   |   **T** | 
|has identity   |   F       |   **T**   |   **T**   |   ?     | 
|has inverse    |   F       |   F       |   **T**   |   ?     | 
|is commutative |   ?       |   ?       |   ?       |   **T** | 

## Examples
- Z, Q, R and C are abelian groups under addition,
    and abelian monoids under multiplication, because the
    $0$ doesn't have inverse.
- The set of all $2 x 2$ matrices with real entries
    form a monoid under multiplication (many singular matrices).
- The dihedral group:
    $D_4 = \{I, R, R^2, R^3, T_x, T_y , T_{1,3}, T_{2,4}\}$

## Functor
- Functor is another kind of algebra
- A functor consists of:
    1. A *Container Type* $f$ that holds values of type $a$
    2. A function $fmap$ that takes a function an lifts it
- The Functor Laws
    1. Identity 
        `fmap id == id`
    2. Composition 
        `fmap (f.g) == (fmap f) . (fmap g)`
    3. Structure preservation 
        `fmap :: Functor f => (a -> b) -> f a -> f b`

## Applicative
- The Applicative Laws:
    1. Identity
        `pure id <*> v == v`
    2. Homomorphism
        `pure f <*> pure x == pure (f x)`
    3. Interchange
        `u <*> pure y == pure ($ y) <*> u`
    4. Composition
        `pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`
- Applicatives are more composable than Monads.

## Monads
- The Monad Laws:
    1. Left identity
        `pure <=< f == f`
    2. Right identity
        `f <=< pure == f`
    3. Associativity
        `f <=< (g <=< x)== (f <=< g) <=< x`
- Every Monad is Applicative, and every Applicative is a
    Functor, but said the opposite isn't always true.
- These are similar to the *monoid laws*, but generalized
    for multiple types defined inside the monad. This sort
    of structure is called a *category* in mathematics.

## Summarize
- **Functors** are types for containers where we can `map`
    pure functions on their contents.
- **Applicative Functors** are types where we can combine
    $n$ containers with a $n$-ary function.
- **Monads** are types `m` where we can *sequentially compose*
    functions of the form `a -> m`.
