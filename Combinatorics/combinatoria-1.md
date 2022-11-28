--- title: Combinatoria 1 author: Daniel Sánchez ---

## Reglas de la suma y del producto
- Regla de la suma: Si hay $n(A)$ maneras de hacer $A$ y, distintas de ellas,
- $n(B)$ maneras de hacer $B$, entonces el número de maneras de hacer $A$ o $B$
- es $n(A) + n(B)$. Regla del producto: Si hya $n(A)$ maneras de hacer $A$ y
- $n(B)$ maneras de hacer $B$, entonces el número de maneras de hacer $A$ y $B$
- es $n(A) * n(B)$. Esto es cierto si el números de maneras de hacer $A$ o $B$
- son independientes entre si.

## Funciones básicas

### Permutaciones Una *permutación* es un arreglo de $r$ objetos tomados de un
conjunto de $n$ objetos. El orden importa. El número de permutaciones posibles
es: $$P(n, k) = \frac{n!}{(n-k)!}$$

En general, si existen $n$ objetos con $n_1$ de un primer tipo, $n_2$ de un
segundo tipo y $n_k$ de un $r$-ésimo tipo, dónde $n_1 + n_2 + \dots + n_k = n$,
entonces existen $\frac{n!}{n_1! n_2! \dots n_k}$ disposiciones (lineales) de
los $n$ objetos dados. Los objetos del mismo tipo son indistinguibles.


### Combinaciones Una *combinación* es cualquier subconjunto de $k$ objetos
tomados de un conjunto de $n$ objetos. El orden no importa. El número de
combinaciones posibles es: $$C(n, k) = \binom{n}{k} = \frac{P(n, k)}{k!}$$

El **conjunto potencia** de un conjunto $A$, $\rho(A)$, es el conjunto que
incluye todos los posibles subconjuntos de $A$. Su cardinalidad se calcula
sumando el número de combinaciones que resulta de ir incrementando el tamaño de
los subconjuntos. $$|\rho(A)| = \sum_{k=0}^n \binom{n}{k},n = |A|$$

El **teorema del binomio** describe la expansión algebraica de la $n$-ésima
potencia de un binomio, siendo $n \in \mathbb{Z}^+$. $$(x + y)^n = \sum_{k=0}^n
\binom{n}{k} x^{n-k} y^k$$

Combinando ambos conceptos, se llega a la conclusión de que la cardinalidad del
conjunto potencia de $A$ se puede calcular con $2^n$, siendo $n = |A|$.
$$\begin{aligned}
    (1 + 1)^n   &= \sum_{k=0}^n \binom{n}{k} 1^{n-k} 1^k \\
    2^n         &= \sum_{k=0}^n \binom{n}{k} \\
    2^n         &= |\rho(A)| \\
\end{aligned}$$

$$\log(a, b, p) = \begin{cases}
    0 \quad &, a = 1 \\
    x \quad &, p = 0 \\
    x + \log( , b, p-1) \div 10 \\
\end{cases} \: \text{where} x = \mathrm{BiPLE}(b, a)$$
