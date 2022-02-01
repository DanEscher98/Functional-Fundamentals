module Parser (Tree(..), parse) where

import           Lexer

data Tree
    = FstHierNode Operator Tree Tree
    | SndHierNode Operator Tree Tree
    | UnaryNode Operator Tree
    | AssignNode String Tree
    | NumNode Double
    | VarNode String
    deriving Show

parse :: [Token] -> Tree
parse toks =
    let (tree, toks') = expression toks
     in if null toks'
           then tree
           else error $ "Leftover tokens: " ++ show toks'

expression :: [Token] -> (Tree, [Token])
expression toks =
    let (termTree, toks') = term toks
     in case lookAhead toks' of
          (TokOp op) | elem op [Add, Sub] ->
              let (exTree, toks'') = expression (accept toks')
               in (SndHierNode op termTree exTree, toks'')
          TokAssign ->
              case termTree of
                VarNode str ->
                    let (exTree, toks'') = expression (accept toks')
                     in (AssignNode str exTree, toks'')
                _ -> (termTree, toks')
          _ -> (termTree, toks')

term :: [Token] -> (Tree, [Token])
term toks =
    let (facTree, toks') = factor toks
     in case lookAhead toks' of
          (TokOp op) | elem op [Mul, Div] ->
              let (termTree, toks'') = term (accept toks')
               in (FstHierNode op facTree termTree, toks'')
          _ -> (facTree, toks')

factor :: [Token] -> (Tree, [Token])
factor toks =
    case lookAhead toks of
      (TokNum x) -> (NumNode x, accept toks)
      (TokVar str) -> (VarNode str, accept toks)
      (TokOp op) | elem op [Add, Sub] ->
          let (facTree, toks') = term (accept toks)
           in (UnaryNode op facTree, toks')
      TokLParen ->
          let (expTree, toks') = expression (accept toks)
           in if lookAhead toks' /= TokRParen
                 then error "Missing right parenthesis"
                 else (expTree, accept toks')
      _ -> error $ "Parse error on token: " ++ show toks
