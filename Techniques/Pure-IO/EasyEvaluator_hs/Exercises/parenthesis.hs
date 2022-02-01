module Main where


data Operator = Add | Sub | Mul | Div
    deriving (Show, Eq)

data Tree = SndHierNode Operator Tree Tree
          | FstHierNode Operator Tree Tree
          | AssignNode String Tree
          | UnaryNode Operator Tree
          | NumNode Double
          | VarNode String

instance Show Tree where
    show (SndHierNode Add left right) = bin " + " left right
    show (SndHierNode Sub left right) = bin " - " left right
    show (FstHierNode Mul left right) = bin " * " left right
    show (FstHierNode Div left right) = bin " / " left right
    show (UnaryNode Add tree)         = "( +" ++ show tree ++ ")"
    show (UnaryNode Sub tree)         = "( -" ++ show tree ++ ")"
    show (AssignNode var tree)        =
        "(" ++ var ++ " = " ++ show tree ++ ")"
    show (NumNode x)                  = show x
    show (VarNode var)                = var

bin :: String -> Tree -> Tree -> String
bin op ltree rtree = "(" ++ show ltree ++ op ++ show rtree ++ ")"

-- x = 2 * (y = 5) + 3
testExpr = AssignNode "x" (SndHierNode Add
                             (FstHierNode Mul
                               (NumNode 2.0)
                               (AssignNode "y" (NumNode 5)))
                             (NumNode 3))

main = print $ show testExpr
