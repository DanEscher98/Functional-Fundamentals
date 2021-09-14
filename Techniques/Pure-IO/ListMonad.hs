module ListaMonadica where

data Lista a = Nil | Cons a (Lista a)
    deriving (Show)

concatena :: Lista a -> Lista a -> Lista a
concatena Nil ys         = ys
concatena (Cons x xs) ys = Cons x (concatena xs ys)
