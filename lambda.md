---
title: Collected Lambda Calculus Functions
---

## Collected Lambda Calculus Functions

The following is a small collection of functions in the [untyped lambda
calculus](http://en.wikipedia.org/wiki/Lambda_calculus) which I feel are
noteworthy for one reason or another, either by relevance to the
foundations of lambda calculus (such as the combinators and natural
numbers) or by utility to people who wish to actively make use of this
[Turing tarpit](http://en.wikipedia.org/wiki/Turing_tarpit). Some of
them are taken from Wikipedia (which tends to be very reliable on
mathematical issues), while others (primarily the list functions) I
derived myself.

Unless explicitly noted otherwise, natural numbers, booleans, pairs &
lists, and all functions for dealing with them use the Church encodings
of their values. Operations on other types (e.g., negative or
non-integral numbers) are not defined, and if the reader desires them,
he must construct such types and the operations on them himself
(possibly with the use of pairs acting as typed unions). Additionally,
functions for operating on numbers, lists, etc. are only meant for use
on those types; if a value of the wrong type is supplied (e.g., if a
list is passed to [SUCC]{.L}) or a non-Church encoded value is used, the
results are undefined.

### Common Combinators

- [K := λxy. x ≡ X (X (X X)) ≡ X′ X′ X′]{.L}
- [S := λxyz. (x z) (y z) ≡ X (X (X (X X))) ≡ X K ≡ X′ (X′ X′) ≡ B (B (B W) C) (B B)]{.L}
- [I := λx. x ≡ S K S ≡ S K K ≡ X X]{.L}
- [X := λx. x S K]{.L} --- also called [ι]{.L} (iota)
- [X′ := λx. x K S K]{.L}
- [B := λxyz. x (y z) ≡ S (K S) K]{.L} --- function composition
- [C := λxyz. x z y ≡ S (S (K (S (K S) K)) S) (K K)]{.L}
- [W := λxy. x y y ≡ S S (K (S K K))]{.L}
- [Y := λg. (λx. g (x x)) (λx. g (x x)) ≡ S (K (S I I)) (S (S (K S) K) (K (S I I)))]{.L}
- [Y′ := (λxy. x y x) (λyx. y (x y x)) ≡ S S K (S (K (S S (S (S S K)))) K)]{.L}
- [Θ := (λxy. y (x x y)) (λxy. y (x x y))]{.L} --- called the \"Turing fixed-point combinator\"
- [ω := λx. x x ≡ S I I]{.L}
- [Ω := ω ω]{.L}
- [Ω~2~ := (λx. x x x) (λx. x x x)]{.L}

    A fixed point combinator is any function [F]{.L} for which [F g ≡ g
    (F g)]{.L} for all [g]{.L}; examples include [Y]{.L}, [Y′]{.L}, and
    [Θ]{.L}. Since lambda calculus functions cannot refer to themselves
    by name, fixed point combinators are needed to implement recursion.
    For example, the factorial function can be implemented using [f :=
    λgx. ISZERO x 1 (MULT x (g (PRED x)))]{.L}, which takes a function
    [g]{.L} and a number [x]{.L}; if [x]{.L} is not zero, it is
    multiplied by the result of [g (PRED x)]{.L}. Defining [FACTORIAL :=
    Y f]{.L} (or [Y′ f]{.L} or [Θ f]{.L}) means that [FACTORIAL x ≡ Y f
    x ≡ f (Y f) x]{.L}, and so [f]{.L} is able to recurse on itself
    indefinitely.

#### Natural Numbers

- [0 := λfx. x]{.L}
- [1 := λfx. f x]{.L}
- [2 := λfx. f (f x)]{.L}
- [3 := λfx. f (f (f x))]{.L}
- [4 := λfx. f (f (f (f x)))]{.L}
- [5 := λfx. f (f (f (f (f x))))]{.L}
- *et cetera*

#### Mathematical Operators

-   The successor operator (given a natural number [n]{.math}, calculate
    [n+1]{.math}):

    ::: L
    SUCC := λnfx. f (n f x)
    :::
-   The predecessor operator (for all [n > 0]{.math}, calculate
    [n-1]{.math}; for zero, return zero):
      ------ ---- ----------------------------------------------------------
      PRED   :=   λnfx. n (λgh. h (g f)) (λu. x) (λu. u)
             ≡    λn. n (λgk. ISZERO (g 1) k (PLUS (g k) 1)) (λv. 0) 0
             ≡    λn. CAR (n (λx. PAIR (CDR x) (SUCC (CDR x))) (PAIR 0 0))
      ------ ---- ----------------------------------------------------------
-   Addition:
      ------ ---- --------------------
      PLUS   :=   λmnfx. n f (m f x)
             ≡    λmn. n SUCC m
      ------ ---- --------------------
-   Subtraction --- [SUB m n]{.L} evaluates to [m - n]{.math} if [m >
    n]{.math} and to zero otherwise:

    ::: L
    SUB := λmn. n PRED m
    :::
-   Multiplication:
      ------ ---- -------------------
      MULT   :=   λmnf. m (n f)
             ≡    λmn. m (PLUS n) 0
             ≡    B
      ------ ---- -------------------
-   Division --- [DIV a b]{.L} evaluates to a pair of two numbers, [a
    idiv b]{.math} and [a mod b]{.math}:

    ::: L
    DIV := Y (λgqab. LT a b (PAIR q a) (g (SUCC q) (SUB a b) b)) 0
    :::
-   Integer division:

    ::: L
    IDIV := λab. CAR (DIV a b)
    :::
-   Modulus:

    ::: L
    MOD := λab. CDR (DIV a b)
    :::
-   Exponentiation ([EXP a b ≡ a^b^]{.L}):

    ::: L
    EXP := λab. b a ≡ C I
    :::
-   Factorial:
      ----------- ---- ---------------------------------------------------
      FACTORIAL   :=   Y (λgx. ISZERO x 1 (MULT x (g (PRED x))))
                  ≡    λn. Y (λgax. GT x n a (g (MUL a x) (SUCC x))) 1 1
                  ≡    λn. n (λfax. f (MUL a x) (SUCC x)) K 1 1
      ----------- ---- ---------------------------------------------------
-   Fibonacci numbers --- [FIBONACCI n]{.L} evaluates to the [n]{.L}-th
    Fibonacci number:

    ::: L
    FIBONACCI := λn. n (λfab. f b (PLUS a b)) K 0 1
    :::
-   Greatest common divisor/highest common factor:

    ::: L
    GCD := (λgmn. LEQ m n (g n m) (g m n)) (Y (λgxy. ISZERO y x (g y
    (MOD x y))))
    :::

#### Booleans

Given a boolean value [b]{.L}, the expression [b t f]{.L} will evaluate
to [t]{.L} if [b]{.L} is true and to [f]{.L} if [b]{.L} is false. This
allows conditional expressions to be written simply as a condition
applied directly to the two possible results without the need for an
[IF]{.L} function.

-   [TRUE := λxy. x ≡ K]{.L}
-   [FALSE := λxy. y ≡ 0 ≡ λx. I ≡ K I ≡ S K ≡ X (X X)]{.L}
-   [AND := λpq. p q p]{.L}
-   [OR := λpq. p p q]{.L}
-   [XOR := λpq. p (NOT q) q]{.L}
-   [NOT := λpab. p b a ≡ λp. p FALSE TRUE]{.L}

#### Numeric Comparison Operators

-   Test whether a number is zero:

    ::: L
    ISZERO := λn. n (λx. FALSE) TRUE
    :::
-   Less than:

    ::: L
    LT := λab. NOT (LEQ b a)
    :::
-   Less than or equal to:

    ::: L
    LEQ := λmn. ISZERO (SUB m n)
    :::
-   Equal to:

    ::: L
    EQ := λmn. AND (LEQ m n) (LEQ n m)
    :::
-   Not equal to:

    ::: L
    NEQ := λab. OR (NOT (LEQ a b)) (NOT (LEQ b a))
    :::
-   Greater than or equal to:

    ::: L
    GEQ := λab. LEQ b a
    :::
-   Greater than:

    ::: L
    GT := λab. NOT (LEQ a b)
    :::

#### Pairs and Lists

Pairs and lists are structured the same way that they are in Lisp and
its relatives: a pair is made up of two components, called the *car* and
the *cdr*, and a list is either [NIL]{.L} (the empty list) or a pair
whose cdr is another list (and whose car is an element of the enclosing
list).

-   [PAIR x y]{.L} --- create a pair with a car of [x]{.L} and a cdr of
    [y]{.L}; also called [CONS]{.L}:

    ::: L
    PAIR := λxyf. f x y
    :::
-   [CAR p]{.L} --- get the car of pair [p]{.L}; also called [FIRST]{.L}
    or [HEAD]{.L}:

    ::: L
    CAR := λp. p TRUE
    :::
-   [CDR p]{.L} --- get the cdr of pair [p]{.L}; also called
    [SECOND]{.L}, [TAIL]{.L}, or [REST]{.L}:

    ::: L
    CDR := λp. p FALSE
    :::
-   The empty list:

    ::: L
    NIL := λx. TRUE
    :::
-   [NULL p]{.L} --- evaluates to [TRUE]{.L} if [p]{.L} is [NIL]{.L} or
    to [FALSE]{.L} if [p]{.L} is a normal pair (all other types are
    undefined):

    ::: L
    NULL := λp. p (λxy. FALSE)
    :::

#### List Functions

-   Concatenate two lists:

    ::: L
    APPEND := Y (λgab. NULL a b (PAIR (CAR a) (g (CDR a) b)))
    :::
-   Calculate the length of a list:

    ::: L
    LENGTH := Y (λgcx. NULL x c (g (SUCC c) (CDR x))) 0
    :::
-   [INDEX x i]{.L} --- evaluates to the [i]{.L}-th (zero-based) element
    of list [x]{.L}, assuming that [x]{.L} has at least [i+1]{.math}
    elements:

    ::: L
    INDEX := λxi. CAR (i CDR x)
    :::
-   Get the last element in a list:

    ::: L
    LAST := Y (λgx. NULL x NIL (NULL (CDR x) (CAR x) (g (CDR x))))
    :::
-   Get a list without its last element:

    ::: L
    TRUNCATE := Y (λgx. NULL x NIL (NULL (CDR x) NIL (PAIR (CAR x) (g
    (CDR x)))))
    :::
-   Reverse a list:

    ::: L
    REVERSE := Y (λgal. NULL l a (g (PAIR (CAR l) a) (CDR l))) NIL
    :::
-   [RANGE s e]{.L} --- evaluates to a list of all natural numbers from
    [s]{.L} up through [e]{.L}, or to [NIL]{.L} when [s > e]{.math}.

    ::: L
    RANGE := λse. Y (λgc. LEQ c e (PAIR c (g (SUCC c) e)) NIL) s
    :::
-   [LIST n a~0~ a~1~ \... a~n-1~]{.L} --- evaluates to [a~0~ \...
    a~n-1~]{.L} as a list

    ::: L
    LIST := λn. n (λfax. f (PAIR x a)) REVERSE NIL
    :::
-   [APPLY f x]{.L} --- passes the elements of the list [x]{.L} to
    [f]{.L}:

    ::: L
    APPLY := Y (λgfx. NULL x f (g (f (CAR x)) (CDR x)))
    :::
-   [MAP f x]{.L} --- maps each element of the list [x]{.L} through
    [f]{.L}:

    ::: L
    MAP := Y (λgfx. NULL x NIL (PAIR (f (CAR x)) (g f (CDR x))))
    :::
-   [FILTER f x]{.L} --- evaluates to a list of all [e]{.L} in the list
    [x]{.L} for which [f e]{.L} is [TRUE]{.L} (assuming that [f]{.L}
    returns only [TRUE]{.L} or [FALSE]{.L} for all elements of [x]{.L}):

    ::: L
    FILTER := Y (λgfx. NULL x NIL (f (CAR x) (PAIR (CAR x)) I (g f (CDR
    x))))
    :::
-   [CROSS f l m]{.L} --- evaluates to a list of all values of [f a
    b]{.L} where [a]{.L} is in the list [l]{.L} and [b]{.L} is in the
    list [m]{.L}. To obtain the Cartesian cross product of [l]{.L} and
    [m]{.L}, supply [PAIR]{.L} (or a similar function) for [f]{.L}.

    ::: L
    CROSS := λflm. FOLD-LEFT APPEND NIL (MAP (λx. MAP (f x) m) l)
    :::
-   [FOLD-LEFT f e x]{.L} --- Apply [f a]{.L} to each element of the
    list [x]{.L}, where [a]{.L} is the result of the previous
    application (or [e]{.L} for the first application) and return the
    result of the last application:

    ::: L
    FOLD-LEFT := Y (λgfex. NULL x e (g f (f e (CAR x)) (CDR x)))
    :::
-   [FOLD-RIGHT f e x]{.L} --- Apply [(λy. f y a)]{.L} to each element
    of the list [x]{.L} in reverse order, where [a]{.L} is the result of
    the previous application (or [e]{.L} for the first application) and
    return the result of the last application:

    ::: L
    FOLD-RIGHT := λfex. Y (λgy. NULL y e (f (CAR y) (g (CDR y)))) x
    :::

#### Other

-   [GET n i a~0~ a~1~ \... a~n-1~ =^β^ a~i~]{.L}:

    ::: L
    GET := λni. i K (SUB n (SUCC i) K)
    :::

#### Sources

-   [Wikipedia: Lambda
    calculus](http://en.wikipedia.org/wiki/Lambda_calculus)
-   [Wikipedia: Combinatory
    logic](http://en.wikipedia.org/wiki/Combinatory_logic)
-   [Wikipedia: SKI combinator
    calculus](http://en.wikipedia.org/wiki/SKI_combinator_calculus)
-   [Wikipedia: Fixed point
    combinator](http://en.wikipedia.org/wiki/Fixed_point_combinator)
-   [Wikipedia: B,C,K,W
    system](http://en.wikipedia.org/wiki/B,C,K,W_system)

[Main Page](index.html)

\$Id: lambda.html,v 1.3 2014/06/23 01:42:31 jwodder Exp jwodder \$
:::
