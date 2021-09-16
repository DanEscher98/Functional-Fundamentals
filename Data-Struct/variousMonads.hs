module ListaMonadica where

data Lista a = Nil | Cons a (Lista a)

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

-- # -- # -- # -- #
data Alguno a = Fallo a | Exito a

instance Show a => Show (Alguno a) where
    show (Fallo a) = "0_0 " ++ show a
    show (Exito a) = "^_^ " ++ show a

instance Functor Alguno where
    fmap f (Fallo a) = Fallo a
    fmap f (Exito a) = Exito (f a)

instance Applicative Alguno where
    pure = Exito
    Fallo a <*> _ = Fallo a
    _ <*> Fallo a = Fallo a
    Exito f <*> Exito a = Exito (f a)

instance Monad (Alguno a) where
    return a = Exito a
    Fallo a >>= _ = Fallo a
    Exito a >>= f = Exito (f a)
