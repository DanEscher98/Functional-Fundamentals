module ErrorEncapsulated where

import           Control.Exception (SomeException, evaluate, try)
import           Control.Monad     ((<=<))
import           Data.Char         (isSpace)
import           System.IO         (hFlush, stdout)
import           System.IO.Unsafe  (unsafePerformIO)

prompt :: String -> IO String
prompt text = putStr text >> hFlush stdout >> getLine

maybeRead :: Read a => String -> Maybe a
maybeRead s = let s' = takeWhile (not . isSpace) s
               in case reads s' of
                    [(x, "")] -> Just x
                    _         -> Nothing

getValue :: Read a => String -> IO a
getValue msg = do -- tryExpr . read == maybeRead
    input <- (return . tryExpr . read) <=< prompt $ msg
    case input of
      Nothing -> getValue "Try again: "
      Just n  -> return n

-- https://youtu.be/Nr_miheqFU4?t=32
tryExpr :: forall a . a -> Maybe a
tryExpr expr = unsafePerformIO $ do
    result <- (try . evaluate) expr :: IO (Either SomeException a)
    case result of
        Left _  -> return Nothing
        Right x -> return $ Just x

-- https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell
