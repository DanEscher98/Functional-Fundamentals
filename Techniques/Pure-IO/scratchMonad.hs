module MyFunk (
    unboxStr, putUnBox,
    say1, say2, say3, say4,
--    list1, list2, list3, list4
    ) where

-- (<$>) :: Functor f       =>   (a -> b) -> f a -> f b
-- (<*>) :: Applicative f   => f (a -> b) -> f a -> f b

unboxStr :: Maybe String -> String
unboxStr (Just text) = text
unboxStr Nothing     = ""

putUnBox :: Maybe String -> IO ()
putUnBox (Just text) = putStrLn text
putUnBox Nothing     = return ()

say1 :: String -> String
say1 text = "Hello " ++ text
-- say1 <$> (Just "John")           --> Just "Hello John"
-- say1 <$> ["J", "M"]              --> ["Hello J", "Hello M"]
-- (Just say1) <*> (Just "World")   --> Just "Hello Mary"
-- [say1] <*> ["J", "M"]            --> ["Hello J", "Hello M"]

say2 :: String -> String -> String
say2 txt1 txt2 = txt1 ++ " " ++ txt2
-- (say2 "Hi") <$> (Just "John")    --> Just "Hi John"
-- (say2 "Hello") <$> ["Jo", "Ma"]  --> ["Hi Jo", "Hi Ma"]
-- fmap (say2 "Hi") (Just "World")  --> Just "Hi World"
-- fmap (say2 "Hi") ["Jo", "Ma"]    --> ["Hi Jo", "Hi Ma"]
-- Just (say2 "Hi") <*> (Just "Mu") --> Just "Hi Mu"
-- [say2 "Hi"] <*> ["World", "Ada"] --> ["Hi World", "Hi Ada"]
-- say2 <$> ["A", "B"] <*> ["1", "2"] > ["A1", "A2", "B1", "B2"]
-- (+) <$> Just 3 <*> Just 5        --> Just 8

say3 :: String -> Maybe String
say3 text
    | not . null $ text = Just ("Hello " ++ text)
    | otherwise         = Nothing
-- (Just "World") >>= say3

say4 :: String -> String -> Maybe String
say4 text greet
    | (not . any null) [text, greet]    = Just (greet ++ text)
    | otherwise                         = Nothing
-- Just "Hi " >>= say4 "pal," >>= say4 " How are you?"
