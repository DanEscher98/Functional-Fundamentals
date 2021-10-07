module JsonParser where

import           Data.Char

data JsonValue
    = JsonNull
    | JsonBool      Bool
    | JsonNum       Int
    | JsonString    String
    | JsonArray     [JsonValue]
    | JsonObject    [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) = Parser $
        \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $
        \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

charP :: Char -> Parser Char
charP x = Parser f where
    f []        = Nothing
    f (y:ys)    | y == x    = Just (ys, x)
                | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"
-- jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined
