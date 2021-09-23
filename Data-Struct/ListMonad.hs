module ListaMonadica where

data Lista a = Nil | Cons a (Lista a)

instance (Show a) => Show (Lista a) where
    show Nil         = ""
    show (Cons x xs) = show x ++ " " ++ show xs

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

-- Pythagorean triples
triplesM = do
    z <- [1..]
    x <- [1..z]
    y <- [x..z]
    guard (x^2 + y^2 == z^2)
    return (x, y, z)

guard :: Bool -> [()]
guard True  = [()]
guard False = []


triplesC = [(x, y, z)   | z <- [1..]
                        , x <- [1..z]
                        , y <- [x..z]
                        , x^2 + y^2 == z^2]
