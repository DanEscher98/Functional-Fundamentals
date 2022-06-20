module ListaMonadica where

data Lista a = Nil | Cons a (Lista a)

data Product a b = Pair { fst::a, snd::b }

instance (Show a) => Show (Lista a) where
    show Nil         = ""
    show (Cons x xs) = show x ++ " " ++ show xs

instance Functor Lista where
    fmap f Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Lista where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons x xs =
        Cons (f x) (concatena (fmap f xs) (fs <*> xs))

instance Monad Lista where
    return x = Cons x Nil
    xs >>= k = unir $ fmap k xs

concatena :: Lista a -> Lista a -> Lista a
concatena Nil ys         = ys
concatena (Cons x xs) ys = Cons x (concatena xs ys)

unir :: Lista (Lista a) -> Lista a
unir Nil           = Nil
unir (Cons xs xss) = concatena xs (unir xss)

cabeza :: Lista a -> a
cabeza Nil        = error "Lista vacia"
cabeza (Cons x _) = x

cola :: Lista a -> Lista a
cola Nil         = error "Lista vacia"
cola (Cons _ xs) = xs

-- # -- # -- # -- #

data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = "¯\\_(ツ)_/¯"
    show (Un x) = "(✿ ◠‿◠)-☆ " ++ show x

instance Functor Quizas where
    fmap f Nada   = Nada
    fmap f (Un x) = Un (f x)

instance Applicative Quizas where
    pure = Un
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    return = Un
    Nada >>= _ = Nada
    Un a >>= f = f a
-- instance Monad Maybe where
-- Nothing >>= func = Nothing
-- Just val >>= func = func val
