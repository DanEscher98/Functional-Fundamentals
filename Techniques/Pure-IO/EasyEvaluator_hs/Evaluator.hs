module Evaluator where

import qualified Data.Map as Map
import           Lexer
import           Parser

type SymTab = Map.Map String Double

evaluate :: Tree -> SymTab -> (Double, SymTab)
evaluate (SndHierNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
     in case op of
          Add -> (lft + rgt, symTab'')
          Sub -> (lft - rgt, symTab'')
evaluate (FstHierNode op left right) symTab =
    let (lft, symTab') = evaluate left symTab
        (rgt, symTab'') = evaluate right symTab'
     in case op of
          Mul -> (lft * rgt, symTab)
          Div -> (lft / rgt, symTab)
evaluate (UnaryNode op tree) symTab =
    let (x, symTab') = evaluate tree symTab
     in case op of
          Add -> (x, symTab')
          Sub -> (-x, symTab')
evaluate (NumNode x) symTab = (x, symTab)
evaluate (VarNode str) symTab = lookUp str symTab
evaluate (AssignNode str tree) symTab =
    let (val, symTab') = evaluate tree symTab
        (_, symTab'') = addSymbol str val symTab'
     in (val, symTab'')


lookUp :: String -> SymTab -> (Double, SymTab)
lookUp str symTab =
    case Map.lookup str symTab of
      Just val -> (val, symTab)
      Nothing  -> error $ "Undefined variable " ++ str

addSymbol :: String -> Double -> SymTab -> ((), SymTab)
addSymbol str val symTab =
    let symTab' = Map.insert str val symTab
     in ((), symTab')
