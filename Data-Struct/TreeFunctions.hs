module TreeFunctions where

data Tree a = Leaf a | Node a (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
    show (Leaf a)   = "[" ++ show a ++ "]"
    show (Node v l r) =
        "(" ++ show l ++ " -["
        ++ show v ++
        "]- " ++ show r ++ ")"

aTree :: (Tree Int)
aTree = Node 3  (Node 4 (Leaf 2) (Leaf 4))
                (Node 5 (Leaf 1) (Leaf 6))

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem (Leaf x) e     = e == x
treeElem (Node x l r) e =
    case compare e x of
        EQ -> True
        LT -> treeElem l e
        GT -> treeElem r e

data ArbolB a = Hoja a | Rama (ArbolB a) (ArbolB a)
instance (Show a) => Show (ArbolB a) where
    show (Hoja x)   = "(" ++ show x ++ ")"
    show (Rama i d) =
        "[" ++ show i
        ++ "|·|" ++
        show d ++ "]"

unArbol :: (ArbolB Int)
unArbol = Rama  (Rama (Hoja 1) (Hoja 2))
                (Rama (Hoja 3) (Hoja 4))

nHojas :: ArbolB a -> Int
nHojas (Hoja _)   = 1
nHojas (Rama i d) = nHojas i + nHojas d

balanceado :: ArbolB a -> Bool
balanceado (Hoja _) = True
balanceado (Rama i d) = abs (nHojas i - nHojas d) <= 1
                        && balanceado i
                        && balanceado d

lista2Balanceado :: [a] -> ArbolB a
lista2Balanceado [x] = Hoja x
lista2Balanceado xs = Rama (lista2Balanceado ms) (lista2Balanceado ns)
    where (ms, ns) = splitAt (length xs `div` 2) xs

data ArbolN a = Null | Branch a (ArbolN a) (ArbolN a)
    deriving (Show)

numHojas :: ArbolN a -> Int
numHojas Null           = 1
numHojas (Branch _ l r) = numHojas l + numHojas r

numNodos :: ArbolN a -> Int
numNodos Null           = 0
numNodos (Branch _ l r) = 1 + numNodos l + numNodos r

scanArbol :: ArbolB a -> (Int, Int)
scanArbol (Hoja _) = (1, 0)
scanArbol (Rama l r) = (hojas_l + hojas_r, 1 + ramas_l + ramas_r)
    where (hojas_l, ramas_l) = scanArbol l
          (hojas_r, ramas_r) = scanArbol r

profundidad :: ArbolN a -> Int
profundidad Null           = 0
profundidad (Branch _ l r) = 1 + max (profundidad l) (profundidad r)

-- Recorrido
-- Raíz, subárbol izquierdo y luego el derecho
preorden :: ArbolN a -> [a]
preorden Null           = []
preorden (Branch x l r) = x : (preorden l ++ preorden r)

-- Subárbol izquierdo, luego el derecho y al último la raíz
postorden :: ArbolN a -> [a]
postorden Null           = []
postorden (Branch x l r) = postorden l ++ postorden r ++ [x]

-- Usando un acumulador
preordenIter :: ArbolN a -> [a]
preordenIter x = auxLoop x []
    where auxLoop Null xs = xs
          auxLoop (Branch x l r) xs =
              x : auxLoop l (auxLoop r xs)

espejo :: ArbolN a -> ArbolN a
espejo Null           = Null
espejo (Branch x l r) = Branch x (espejo r) (espejo l)

takeTree :: Int -> ArbolN a -> ArbolN a
takeTree 0 _              = Null
takeTree _ Null           = Null
takeTree n (Branch x l r) =
    Branch x (takeTree (pred n) l) (takeTree (pred n) r)

repeatTree :: a -> ArbolN a
repeatTree e = Branch e t t where
    t = repeatTree e

replicateTree :: Int -> a -> ArbolN a
replicateTree 0 _ = Null
replicateTree n e = Branch e t t where
    t = replicateTree (pred n) e

data Direction = LeftD | RightD
    deriving (Eq)

listFromBranch :: Direction -> ArbolN a -> [a]
listFromBranch _ Null = []
listFromBranch d (Branch x l r)
    | d == LeftD    = x : listFromBranch d l
    | otherwise     = x : listFromBranch d r

