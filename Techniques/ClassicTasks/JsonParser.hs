module JsonParser where

import           Control.Applicative
import           Data.Char

    {-
        # TODO ########################
        - proper error report
        - support for float numbers
        - support for scape character
        -}


-- ----------------------------------------------
-- ## Data Types ################################
data JsonValue
    = JsonNull
    | JsonBool      Bool
    | JsonNumber    Int     -- NOTE: no suppor for floats
    | JsonString    String
    | JsonArray     [JsonValue]
    | JsonObject    [(String, JsonValue)]
    deriving (Show, Eq)

-- NOTE: no proper error reporting
newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a) }


-- ----------------------------------------------
-- ## Definition of instances ###################
instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input


-- ----------------------------------------------
-- ## Primitive Parsers #########################
charP :: Char -> Parser Char
charP x = Parser f where
    f []        = Nothing
    f (y:ys)    | y == x    = Just (ys, x)
                | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
           then Nothing
           else Just (input', xs)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element =
    (:) <$> element <*> many (sep *> element) <|> pure []

ws :: Parser String
ws = spanP isSpace


-- ----------------------------------------------
-- ## Parsers ###################################
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"
-- jsonNull = (\_ -> JsonNull) <$> stringP "null"

jsonBool :: Parser JsonValue
jsonBool = JsonBool
    <$> ((True <$ stringP "true")
    <|>  (False <$ stringP "false"))
-- jsonBool = f <$> (stringP "true" <|> stringP "false")
--     where f "true"  = JsonBool True
--           f "false" = JsonBool False
--           f _       = undefined

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f digits = JsonNumber $ read digits

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> elements <* charP ']') where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (
    charP '{' *> ws *>
    sepBy (ws *> charP ',' <* ws) pair
    <* ws <* charP '}')
    where
        pair =
            (\key _ value -> (key, value)) <$> stringLiteral <*>
            (ws *> charP ':' <* ws) <*>
            jsonValue
        -- stringLiteral is used because JsonString is not a type

jsonValue :: Parser JsonValue
jsonValue
  =     jsonNull
  <|>   jsonBool
  <|>   jsonNumber
  <|>   jsonString
  <|>   jsonArray
  <|>   jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)

-- Just (JsonObject xs) <- parseFile "./file.json" jsonValue
-- let JsonArray ys = snd $ head xs
-- map (\(JsonObject xs) -> print $ lookup "name" xs) ys

main :: IO ()
main = undefined
