module Main where

import           Data.Char
import qualified Data.Map  as Map


data Operator = Add | Sub | Mul | Div
    deriving (Show, Eq)

data Token
    = TokOp Operator
    | TokAssign
    | TokLParen
    | TokRParen
    | TokIdent String
    | TokNum Double
    | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c = case c of
               '+' -> Add
               '-' -> Sub
               '*' -> Mul
               '/' -> Div

tokenize :: String -> [Token]
tokenize [] = []


main :: IO ()
main = do
    loop (Map.fromList [("pi", pi)])


type SymTab = Map.Map String Double

loop :: SymTab -> IO ()
loop symtab = do
    str <- getLine
    if null str
       then return ()
       else let toks = tokenize str
                tree = parse toks
                (val, symtab') = evaluate tree symtab
             in do
                 print val
                 loop symtab'


lookUp :: String -> SymTab -> (Double, SymTab)
lookUp str symtab =
    case Map.lookup str symtab of
      Just val -> (val, symtab)
      Nothing  -> error $ "Undefined variable" ++ str

addSymbol :: String -> Double -> SymTab -> ((), SymTab)
addSymbol str val symtab =
    let symtab' = Map.insert str val symtab
     in ((), symtab')

evaluate :: Tree -> SymTab -> (a, SymTab)
evaluate (SumNode op left right) symtab =
    let (lft, symtab') = evaluate left symtab
        (rgt, symtab'') = evaluate right symtab'
     in case op of
         Add -> (lft + rgt, symtab'')
         Sub -> (lft - rgt, symtab'')
evaluate (VarNode str symtab) = lookUp str symtab
evaluate (AssignNode str tree) symtab =
    let (val, symtab') = evaluate tree symtab
        (_, symtab'') = addSymbol str val symtab'
     in (val, symtab'')
