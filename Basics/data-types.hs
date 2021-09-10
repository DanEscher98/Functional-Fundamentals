module MyData where


newtype Conjunto a = Conjunto [a] deriving(Eq, Ord, Show)

vacio :: Conjunto a
vacio = Conjunto []

añade :: Eq a => Conjunto a -> a -> Conjunto a
añade (Conjunto xs) x
    | x `notElem` xs = Conjunto (x:xs)
    | otherwise = Conjunto xs


