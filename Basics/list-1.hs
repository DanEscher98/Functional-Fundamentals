module ListasBasicas where
{-
funciones implementadas:
    head, tail, init, last, drop, take, reverse
    dropWhile, takeWhile, filter, splitAt
    elem, noElem, all, any, filter, length
    foldl, foldr, scanr, map, iterate, until
    repeat, replicate, zip, zipWith
-}

-- Equivalente a head
cabeza :: [a] -> a
cabeza []     = error "Es una lista vacía"
cabeza (x:xs) = x

-- Equivalente a tail
cola :: [a] -> [a]
cola []     = error "Es una lista vacía"
cola (x:xs) = xs

-- Equivalente a init
iniciales :: [a] -> [a]
iniciales []     = error "Es una lista vacía"
iniciales [a]    = []
iniciales (x:xs) = x : iniciales xs

-- Equivalente a last
ultimos :: [a] -> a
ultimos []     = error "Es una lista vacía"
ultimos [x]    = x
ultimos (x:xs) = ultimos xs

-- Equivalente a take
toma :: Int -> [a] -> [a]
toma 0 _    = []
toma _ []   = []
toma n (x:xs)
    | n > 0 = x : toma (pred n) xs
toma _ _    = error "Argumento negativo"

-- Equivalente a drop
tira :: Int -> [a] -> [a]
tira _ [] = []
tira n (x:xs)
    | n >  0  = tira (pred n) xs
    | n == 0  = xs
tira _ _  = error "Argumento negativo"

-- Equivalente a reverse
revertir :: [a] -> [a]
revertir []     = []
revertir (x:xs) = revertir xs ++ [x]

-- Equivalente a splitAt
separaEn :: Int -> [a] -> ([a], [a])
separaEn n l = auxSplit n ([], l) where
    auxSplit 0 p         = p
    auxSplit _ (a, [])   = (a, [])
    auxSplit n (a, x:xs) = auxSplit (pred n) (a ++ [x], xs)

-- Equivalente a dropWhile
tiraMientras :: (a -> Bool) -> [a] -> [a]
tiraMientras _ [] = []
tiraMientras p (x:xs)
    | p x       = tiraMientras p xs
    | otherwise = xs

-- Equivalente a takeWhile
tomaMientras :: (a -> Bool) -> [a] -> [a]
tomaMientras _ [] = []
tomaMientras p (x:xs)
    | p x       = x : tomaMientras p xs
    | otherwise = []

-- Equivalente a elem
elemento :: (Eq a) => a -> [a] -> Bool
elemento _ [] = False
elemento a (x:xs)
    | a == x    = True
    | otherwise = elemento a xs

-- Equivalente a noElem
noElemento :: (Eq a) => a -> [a] -> Bool
noElemento _ [] = True
noElemento a (x:xs)
    | a == x    = False
    | otherwise = noElemento a xs

-- Equivalente a length
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = succ (longitud xs)

-- Equivalente a filter
filtroR :: (a -> Bool) -> [a] -> [a]
filtroR _ [] = []
filtroR f (x:xs)
    | f x       = x : filtroR f xs
    | otherwise = filtroR f xs

filtroC :: (a -> Bool) -> [a] -> [a]
filtroC f xs = [x | x <- xs, f x]

-- Equivalente a all
todosC :: (a -> Bool) -> [a] -> Bool
todosC f xs = and [f x | x <- xs]

todosR :: (a -> Bool) -> [a] -> Bool
todosR _ []     = False
todosR f [x]    = f x
todosR f (x:xs) = f x && todosR f xs

-- Equivalente a any
algunoC :: (a -> Bool) -> [a] -> Bool
algunoC f xs = or [f x | x <- xs]

algunoR :: (a -> Bool) -> [a] -> Bool
algunoR _ []     = False
algunoR f [x]    = f x
algunoR f (x:xs) = f x || algunoR f xs

-- Equivalente a foldl
pliegaI :: (a -> a -> a) -> a -> [a] -> a
pliegaI _ e []     = e
pliegaI f e (x:xs) = pliegaI f (f e x) xs

-- Equivalente a foldr
pliegaD :: (a -> a -> a) -> a -> [a] -> a
pliegaD f e l = pliegaI f e (reverse l)

-- Equivalente a scanr
escaneaI :: (a -> a -> a) -> a -> [a] -> [a]
escaneaI _ e []     = [e]
escaneaI f e (x:xs) = v : escaneaI f v xs
    where v = f e x

-- Equivalente a map
mapeaR :: (a -> b) -> [a] -> [b]
mapeaR _ []     = []
mapeaR f (x:xs) = f x : mapeaR f xs

mapeaC :: (a -> b) -> [a] -> [b]
mapeaC f xs = [f e | e <- xs]

-- Equivalente a iterate
itera :: (a -> a) -> a -> [a]
itera f x = e : itera f e
    where e = f x

-- Equivalente a until
hastaQue :: (a -> Bool) -> (a -> a) -> a -> a
hastaQue p f a
    | p a       = a
    | otherwise = hastaQue p f (f a)

-- Equivalente a repeat
repite :: a -> [a]
repite a = [a | _ <- [0..]]

-- Equivalente a replicate
replicaR :: Int -> a -> [a]
replicaR 0 _ = []
replicaR n a = a : replicaR (pred n) a

replicaC :: Int -> a -> [a]
replicaC 0 _ = []
replicaC n a = [a | _ <- [0..n]]

-- Equivalente a zip
cierre :: [a] -> [b] -> [(a, b)]
cierre _ []          = []
cierre [] _          = []
cierre (x:xs) (y:ys) = (x, y) : cierre xs ys

-- Equivalente a zipWith
cierreCon :: (a -> b -> c) -> [a] -> [b] -> [c]
cierreCon _ _ []          = []
cierreCon _ [] _          = []
cierreCon f (x:xs) (y:ys) = f x y : cierreCon f xs ys
