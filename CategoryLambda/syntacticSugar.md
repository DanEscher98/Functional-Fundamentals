---
title: Syntactic Sugar in Haskell
author: Daniel Sanchez
---

## Miscellaneous

Syntactic   | Evaluation
----------- | -----------
`(+x)`      | `flip (+) x`

## Lists

Syntactic   | Evaluation
----------- | ----------
`[a]`       | `Lista a`
`[0..2]`    | `[0,1,2]`
`[1,2,3]`   | `1:2:3:[]`
`[f e | e <- xs]` | `map f xs`

## Guards

The expression

```haskell
funk x
    | even x    = 1
    | otherwise = 0
```

is evaluated as:

```haskell
funk x = 
    if even x
        then 1
        else 0
```

## Do notation

```haskell
main = do
    s <- getStr
    putStr s
    putStr "Hi"
```

is evaluated as:

```haskell
main = getStr >>=
    (\s -> putStr s >> 
    (putStr "Hi"))
```
