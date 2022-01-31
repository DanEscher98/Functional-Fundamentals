module TiposDefinidos where

data Natural = Zero | Succ Natural
    deriving (Eq, Show)

suma :: Natural -> Natural -> Natural
suma Zero n     = n
suma m Zero     = m
suma (Succ m) n = suma m (Succ n)

mult :: Natural -> Natural -> Natural
mult Zero _        = Zero
mult (Succ Zero) n = n
mult (Succ m) n    = suma n (mult m n)


data Arbol a = Hoja | Fruta a | Rama a (Arbol a) (Arbol a)
    deriving (Eq, Show)

sortListInTree :: Ord a => [a] -> Arbol a
sortListInTree [] = Hoja
sortListInTree (x:xs) = loop xs (Fruta x) where
    loop [] ts     = ts
    loop (x:xs) ts = loop xs (insertInTree x ts)

insertInTree :: Ord a => a -> Arbol a -> Arbol a
insertInTree x Hoja = Fruta x
insertInTree x (Fruta v) = insertInTree x (Rama v Hoja Hoja)
insertInTree x (Rama v l r)
    | x < v     = Rama v (insertInTree x l) r
    | otherwise = Rama v l (insertInTree x r)

inTree :: (Eq a, Ord a) => a -> Arbol a -> Bool
inTree x Hoja         = False
inTree x (Fruta v)    = x == v
inTree x (Rama v l r) = case compare x v of
                          EQ -> True
                          LT -> inTree x l
                          GT -> inTree x r

treeToList :: Arbol a -> [a]
treeToList Hoja         = []
treeToList (Fruta x)    = [x]
treeToList (Rama v l r) =
    (treeToList l) ++
    [v] ++ (treeToList r)

numTerminalNodes :: Arbol a -> Int
numTerminalNodes Hoja         = 0
numTerminalNodes (Fruta _)    = 1
numTerminalNodes (Rama _ l r) =
    numTerminalNodes l + numTerminalNodes r
