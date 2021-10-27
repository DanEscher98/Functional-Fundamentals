---
title: Ecuaciones varias
header-includes:
- \usepackage{ amssymb }
- \usepackage{ mathrsfs }
- \usepackage{ amsmath }
---


## Church Encoding

Esta parte se refiere a los fundamentos del Cálculo Lambda

```haskell
suma :: Int -> Int -> Int
suma a 0 = a
suma a b = succ (suma a (pred b))

```

$$\text{id} \equiv (\lambda x . x)$$
$$\text{F} \equiv (\lambda x y . y)$$
$$\text{T} \equiv (\lambda x y . x)$$
$$\mathcal{L} \mathbb{L} \mathscr{L} \L$$
$$\mathop{\mathcal{L}} [ x ]$$
$$\mathcal{L}^{-1} \left[ \frac{\mathrm{e}^{-as}}{s^3} \right] =
\mathscr{U}(t-a) \mathcal{L}^{-1} \left[ \frac{1}{s^3} \right] \Bigg|_{t \to t-a}$$

$$|x| = \begin{cases}
    x \quad &\text{if} \: x \in \mathbb{Q} \\
    -x \quad &\text{if} \: x \notin \mathbb{Q} \\
\end{cases}$$

$\text{pred}{(n)} = \begin{cases}
    0       &\text{if} \: n = 0 \\
    n - 1   &\text{otherwise} \\
\end{cases}$

$$\text{rem}(a,b) = \begin{cases}
    a                   &\text{if} \: a < b \\
    \text{rem}(a-b,b)   &\text{otherwise}
\end{cases}$$

$$\text{fib}(n) = \begin{cases}
    0                                   &\text{if}\: n=0 \\
    1                                   &\text{if}\: n=1 \\
    \text{fib}(n-1) + \text{fib}(n-2)   &\text{otherwise}
\end{cases}$$

$$n! = \begin{cases}
    1           &,\: n \leq 1 \\
    n*(n-1)!
\end{cases}$$

$$n! = \begin{cases}
    1           \quad&\text{if}\: n \leq 1 \\
    n*(n-1)!    \quad&\text{otherwise}
\end{cases}$$

$$\text{IsOrdered}(l) = \begin{cases}
    \text{True}     &,\: \text{IsEmpty}(ls) \\
    \text{False}    &,\: a > b \\
    \text{IsOrdered}(ls)
\end{cases} \\
\text{where} \quad \begin{aligned}
    a   &= \text{Head}(l) \\
    ls  &= \text{Tail}(l) \\
    b   &= \text{Head}(ls)
\end{aligned}$$

$$a*n = \underbrace{a + a + \dots + a}_{n}$$
