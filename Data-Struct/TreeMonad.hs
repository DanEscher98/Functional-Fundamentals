module TreeMonad where

import           Control.Applicative

-- Good as Functor, Bad as and Applicative or Monad
data SimpleTree a
    = SLeaf
    | SNode a (SimpleTree a) (SimpleTree a)
    deriving (Show)

instance Functor SimpleTree where
    fmap _ SLeaf = SLeaf
    fmap f (SNode x lt rt) =
        SNode (f x) (fmap f lt) (fmap f rt)

-- Good as Functor and Monad, Bad as Applicative
data BinaryTree a
    = BFruit a
    | BNode (BinaryTree a) (BinaryTree a)
    deriving (Show)

instance Functor BinaryTree where
    fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
    fmap f (BFruit x) = BFruit (f x)
    fmap f (BNode lt rt) =
        BNode (fmap f lt) (fmap f rt)

instance Applicative BinaryTree where
    pure x = BFruit x
    (<*>) :: BinaryTree (a -> b) -> BinaryTree a -> BinaryTree b
    (BFruit f) <*> (BFruit x)    = BFruit (f x)
    (BFruit f) <*> (BNode lt rt) = BNode (fmap f lt) (fmap f rt)
    -- It doesn't make sense to all cases

instance Monad BinaryTree where
    (BFruit x) >>= f    = f x
    (BNode lt rt) >>= f = BNode (lt >>= f) (rt >>= f)

-- Good as Functor, Applicative and Monad
data MultiTree a = MNode a [MultiTree a] deriving (Show)

instance Functor MultiTree where
    fmap f (MNode x ts) = MNode (f x) (map (fmap f) ts)

instance Applicative MultiTree where
    pure x = MNode x []
    (MNode f tfs) <*> (MNode x txs) =
        MNode (f x) (
            (map (fmap f) txs) ++ map (<*> (MNode x txs)) tfs)

instance Monad MultiTree where
    (MNode x ts) >>= f = MNode x' (ts' ++ map (>>= f) ts)
        where (MNode x' ts') = f x

multiTreeTest :: IO ()
multiTreeTest = do
    let tree1 = (MNode 5 [MNode 3 [], MNode 2 []])
    let tree2 = (MNode 'a' [MNode 'b' [], MNode 'c' []])
    print (fmap (*2) tree1)
    print ((MNode (*2) [MNode (+100) [], MNode (/10) []]) <*> tree1)
    print (tree1 >>= (\x -> MNode (x + 2) []))


    {- REFERENCES
        * https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/
        * -}
