---
author: Daniel Colin
title: Similarities between Rust and Haskell
---

## Essentially, Traits are Typeclasses

What typeclasses and traits have in common is that they’re used for
all kinds of polymorphism in their respective languages.

### Generics

```rust
// In Rust
fn min<T: Ord>(a: T, b: T) -> T {
    if a > b { b } else { a }
}
```

```haskell
-- In Haskell
min :: (Ord a) => a -> a -> b
min a b
    | a > b     = b
    | otherwise = a
```

### Dynamic Dispatch

Let’s look at runtime polymorphism: the one that OO languages
implement through abstract base classes and virtual methods.

```rust
// Trait definition
trait Draw {
    fn draw(&self);
}

// Data type implementing the trait
struct Circle { radius: i32 }
impl Draw for Circle {
    fn draw(&self) { /* omitted */ }
}

// Usage
fn draw_all(objects: &Vec<Box<Draw>>) {
    for &obj in objects {
        obj.draw();
    }
}
```

In Haskell, the generic function can use typeclass constraints
directly ((Draw a) => ...), but creating a container of different
object types requires a polymorphic wrapper. Note that such containers
aren’t very idiomatic Haskell. A more typical solution would be to
just curry the draw function, implicitly putting the Draw object
inside its closure. 

```haskell
{-# LANGUAGE ExistentialQuantification #-}

-- Typeclass definition
class Draw a where
    draw :: a -> IO ()

-- Polymorphic wrapper type
data Draw' = forall a. Draw a => Draw' a
instance Draw Draw' where
    draw (Draw' d) = draw d

-- Data types instantiating ("implementing") the typeclass
data Circle = Circle ()
instance Draw Circle where draw = undefined -- omitted
data Square = Square ()
instance Draw Square where draw = undefined -- omitted

-- Usage
drawAll :: (Draw a) => [a] -> IO ()
drawAll ds = mapM_ draw ds

main = do
    let shapes = [Draw' Circle (), Draw' Square ()]
    drawAll shapes
```

### Differences

- Rust lacks *higher kinded types*, making certain abstractions
    impossible to encode in traits. However, it's possible to
    implement a trait for infinitely many types at onces if the
    `impl`ementation itself is generic.
- When defining a trait in Rust, you can ask implmentors to provide
    some auxiliary, *associated types* in addition to just methods.
    Haskell could be exanded into *type families* but requires
    enabling a GHC extension.
- In Haskell, typeclasses can be implemented for multiple types
    simultaneously via a GHC extension. On the other hand, Rust makes
    *traits themselves* generic (e.g. `trait Foo<T>`).
- Rust enforces *coherence rules* on trait implementations. Without
    too much detail, coherence demands that there be a local type or
    trait somwhere in th `impl ... for ...` construct. Haskell doesn't
    have this limitation.

## The M-word

They both take advantage of *algebraic data types* (ADT), including
the ability to define both *product types* (regular `struct`s and
`records`) as well as *sum types* (tagged unions). Code in both
languages makes extensive use of the two most basic ADTs:

- `Option` (Rust) or `Maybe` (Haskell) for denoting a presence or
    absence of a value. Is the alternative to *nullable references* by
    encoding the potential nullability into the type system.
    ```rust
    enum Option<T> { Some(T), None }
    ```
    ```haskell
    data Maybe a = Just a | Nothing
    ```

- `Result` (Rust) or `Either` (Haskell) for representing the
    alternative of "correct" and "erroneous" value. In Rust, this
    replaces the traditional error handling mechanisms based on
    exceptions. One thing that Haskell does better is *composing* 
    those fallible functions into bigger chunks of logic. In Rust, the
    `?`operator is the preferred way of error propagation, allowing
    for a more concise composition of functions that return `Result`.
    The `do` notation with `<-` arrows is evidently similar to how in 
    Rust you’d assign the result of a fallible operation after 
    “unpacking” it with ?. But of course, there’s plenty of different
    monads in Haskell: not just IO, but also `Either`, `Maybe`,
    `Reader`, `Writer`, `Cont`, `STM` and many others. In Rust (at
    least as of 1.19), the `?` operator only works for `Result` types

    ```rust
    enum Option<T> { Some(T), None }
    ```
    ```haskell
    data Maybe a = Just a | Nothing
    ```

## References

- (Rust as a gateway drug to Haskell)[http://xion.io/post/programming/rust-into-haskell.html]
