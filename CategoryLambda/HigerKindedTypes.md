Rust/Haskell: Higher-Kinded Types (HKT)
=======================================

A higher kinded type is a concept that reifies a type constructor as an 
actual type.

A type constructor can be thought of in these analogies:

* like a function in the type universe
* as a type with a "hole" in it
* as a container containing type(s)
* as a generic type, parameterised over other types
* as an endofunctor in the category of types

To be able to use them in places where concrete "proper" types are usually 
expected, a language must support the concept of higher kinded types.

There are 3 keywords in "higher kinded types":

1. Higher
2. Kinded
3. Types

**Higher**:

The "higher" prefix is derived from the phrase "higher order". In terms of 
programming, it usually refers to a function that can at least take other 
functions as input, or return functions as output. However I think in this case, 
the prefix has become more general, and refers to a form of recursive 
containment. Basically it's the idea that a type could contain other types. 
See concepts like "higher order messages". This makes sense, and seems to define 
they very concept of type constructors and/or recursive data structures. Here we 
use the word "contain" to mean parameterisation of things related to itself, 
and/or returning of things related to itself. Refer to 1st order and 2nd order 
logic literature for more:

* https://www.academia.edu/11975482/Second-order_logic_explained_in_plain_English_
* http://math.stackexchange.com/questions/1052118/what-are-some-examples-of-third-fourth-or-fifth-order-logic-sentences
* http://programmers.stackexchange.com/questions/186035/who-first-coined-the-term-higher-order-function-and-or-first-class-citizen

**Kinded**:

Kinds are the types of types. Most "proper" types have a kind signature of `*`. 
But higher order types, or type constructors could have a kind signature of 
`* -> *`. The ability of having higher kinds defines a type constructor.

**Types**:

Finally of course I believe this term signifies a reification of our type 
constructors as a first class type that can be manipulated using the same 
tools that we manipulate normal `*` "proper" types.

Haskell
-------

Haskell has good support for higher kinded types. Every type constructor such as 
they `[]` can be used as a "first class type".

This is specifically relevant to typeclasses such as the `Functor` typeclass.

```hs
> :info Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (GHC.Base.<$) :: a -> f b -> f a
        -- Defined in ‘GHC.Base’
instance Functor Maybe -- Defined in ‘Data.Maybe’
instance Functor (Either a) -- Defined in ‘Data.Either’
instance Functor [] -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Functor ((->) r) -- Defined in ‘GHC.Base’
instance Functor ((,) a) -- Defined in ‘GHC.Base’
```

Each of those instances are implementations of the Functor class for a 
particular higher kinded type. For example, we see `[]`, which we know is a type 
constructor but it is also a higher kinded type, since we can use it like a 
"first class type".

Rust
----

Rust 1.0 currently lacks support for higher kinded types. This doesn't mean it 
doesn't have container types or type constructors. It certainly does. It just 
hasn't reified them into a higher kinded type that can be used like a first 
class citizen. It is however on the roadmap.

Just a note about syntax:

* `trait <-> class`
* `impl <-> instance`
* `enum/struct <-> data`

However there are interesting resources for working around this:

* [Using a macro to get HKT!](https://gist.github.com/14427/af90a21b917d2892eace)
* [Using iterators instead!](http://stackoverflow.com/q/18374612/582917)

Other reading:
-------------

* https://en.wikipedia.org/wiki/Lambda_cube
* https://www.academia.edu/11975482/Second-order_logic_explained_in_plain_English_
