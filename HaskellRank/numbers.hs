module Numbers where

import Data.Char

digitalRoot :: (Show a, Integral a) => a -> a
digitalRoot n
  | m < 10 = m
  | otherwise = digitalRoot m
  where m = fromIntegral . sum . map digitToInt $ show n
