# The Easy Evaluator

## Components

1. Lexer: `String -> [Tokens]`
    - define Tokens and Operators data types
    - `case _ of`
2. Parser: `[Tokens] -> AST`
    - define Abstract Syntax Tree
3. Evaluator: `AST -> Computation`
    - define a Symbolic Table for variable values
    - `Data.Map.lookup`
    - `Data.Map.insert`
4. Show: `AST -> String`
