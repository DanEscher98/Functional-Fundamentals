module ListaMonadica where

data Lista a = Nil | Cons a (Lista a)
    deriving (Show)

cabeza :: Lista a -> a
cabeza Nil        = error "Lista vacia"
cabeza (Cons x _) = x

cola :: Lista a -> Lista a
cola Nil         = error "Lista vacia"
cola (Cons _ xs) = xs

concatena :: Lista a -> Lista a -> Lista a
concatena Nil ys         = ys
concatena (Cons x xs) ys = Cons x (concatena xs ys)

a = a
tryLazy 0 _ = 0
tryLazy _ b = b

data Quizas a = Nada | Un a
    deriving (Eq, Ord, Show)
-- instance Monad Maybe where
-- Nothing >>= func = Nothing
-- Just val >>= func = func val
