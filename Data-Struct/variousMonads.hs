module ListaMonadica where

---- POINT
newtype Point a = Point { getTuple :: (a, a) } deriving Show

instance Functor Point where
    fmap :: (a -> b) -> Point a -> Point b
    fmap f (Point (x, y)) = Point (f x, f y)

instance Applicative Point where
    pure x = Point (x, x)
    (<*>) :: Point (a -> b) -> Point a -> Point b
    Point (f, g) <*> Point (m, n) = Point (f m, g n)

instance Monad Point where
    --(>>=) :: Point a -> (a -> Point b) -> Point b
    p >>= f = (\(Point (x, y)) -> Point (fst x, snd y))
        $ fmap (getTuple . f) p

---- LIST
data Lista a = Nil | Cons a (Lista a)

data Product a b = Pair { fst::a, snd::b }

instance (Show a) => Show (Lista a) where
    show Nil         = ""
    show (Cons x xs) = show x ++ " " ++ show xs

instance Functor Lista where
    fmap :: (a -> b) -> Lista a -> Lista b
    fmap f Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Lista where
    pure x = Cons x Nil
    (<*>) :: Lista (a -> b) -> Lista a -> Lista b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> Cons x xs =
        Cons (f x) (concatena (fmap f xs) (fs <*> xs))

instance Monad Lista where
    (>>=) :: Lista a -> (a -> Lista b) -> Lista b
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

dotProduct :: (a -> b -> c) -> [a] -> [b] -> [c]
dotProduct f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

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
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    Nada >>= _ = Nada
    Un a >>= f = f a
-- instance Monad Maybe where
-- Nothing >>= func = Nothing
-- Just val >>= func = func val

data Tree a
    = Leaf
    | Fruit a
    | Node a (Tree a) (Tree a)

instance Functor Tree where
    fmap _ Leaf           = Leaf
    fmap f (Fruit x)      = Fruit (f x)
    fmap f (Node x lt rt) = Node (f x) (fmap f lt) (fmap f rt)

instance Applicative Tree where
    pure x = Fruit x
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Fruit f) <*> (Fruit x) = Fruit (f x)
    (Fruit f) <*> (Node x lt rt) =
        Node (f x) (fmap f lt) (fmap f rt)
    (Node f lt rt) <*> (Fruit x) = Fruit (f x)
    (Node f ltf rtf) <*> (Node x ltx rtx) =
        (Node (f x) (ltf <*> ltx) (rtf <*> rtx))

mergeTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
mergeTree _ Leaf _ = Leaf
mergeTree _ _ Leaf = Leaf
mergeTree f (Fruit x) (Fruit y) = Fruit (f x y)
mergeTree f (Node x _ _) (Fruit y) = mergeTree f (Fruit x) (Fruit y)
mergeTree f (Fruit x) (Node y _ _) = mergeTree f (Fruit x) (Fruit y)
mergeTree f (Node x ltx rtx) (Node y lty rty) =
    Node (f x y) (mergeTree f ltx lty) (mergeTree f rtx rty)

data NryTree a = LeafN a | NodeN a [NryTree a]


newtype Map a b = Map [(a, b)]

instance Functor (Map a) where
    fmap :: (b -> c) -> Map a b -> Map a c
    fmap f (Map ps) = Map (loop f ps) where
        loop _ []          = []
        loop f ((x, y):ps) = (x, f y) : loop f ps

instance Applicative (Map a) where
    pure e = (\s -> Map [(s, e)])

instance Monad (Map a) where
    (>>=) :: Map a b -> ((a, b) -> Map a c) -> Map a c
    (Map []) >>= f = Map []
    (Map p:ps) >>= f = Map (p' ++ ps') where
        Map p' = f p
        Map ps' = Map ps >>= f

addElement :: (a, b) -> Map a b -> Map a b
addElement p (Map ps) = Map (p:ps)

addItemCount :: a -> Map a Int -> Map a Int
addItemCount e (Map []) = Map [(e, 1)]
addItemCount e (Map ((x, i):ms))
    | e == x = Map ((x, succ i):ms)
    | otherwise = addElement (x, i) (addItemCount e (Map ms))

-- countElem :: (Eq a) => [a] -> Map a Int
-- countElem

-- * H F N
-- H . . .
-- F . x x
-- N . x x
