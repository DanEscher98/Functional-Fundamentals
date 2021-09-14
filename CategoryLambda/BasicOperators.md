---
title: Haskell's basic Operators
author: Daniel Sánchez
---

## Mathematics and Lists
- Arithmetic: `+ * - / ^ ^^ **`
- Logical: `&& || == /= < <= >= >`
- Lists: `: ++ !!`

## Functions and Control.Monad
- `(.)` = Function composition
- `($)` = Operator with right associativity and has the lowest precedence
- `($!)` = The same as `($)` but also force strict evaluation
- `(>>)` = Operator `then`, chains two monadic expressions
- `(>>=)` = Operator `bind`, makes available the result of the previous
    computation to the next expression
- `(=<<)` = The same as operator `bind`, but with right associativity
- `(<=<)` = Allows do compositions of monadic computations
- `(>=>)` = The same as `(<=<)` but with left associativity
- `(<$>)` = Functor
- `(<*>)` = Applicative
