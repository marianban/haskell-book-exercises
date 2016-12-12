module Cipher where
  import Data.Char
  ordA = ord 'A'
  ordZ = ord 'Z'
  orda = ord 'a'
  ordz = ord 'z'
  optimize :: Char -> Int -> Int
  optimize y x
    | isUpper y && x > ordZ = ordA + ((x - ordZ - 1) `mod` (ordZ - ordA + 1))
    | isUpper y && x < ordA = ordA + ((x - ordA) `mod` (ordZ - ordA + 1))
    | isLower y && x > ordz = orda + ((x - ordz - 1) `mod` (ordz - orda + 1))
    | isLower y && x < orda = orda + ((x - orda) `mod` (ordz - orda + 1))
    | otherwise = x
  cipher :: Int -> String -> String
  cipher _ [] = []
  cipher offset (x : xs)
    | (not . isChar) x = x : cipher offset xs
    | otherwise = (chr . (optimize x) . (+offset) . ord $ x) : cipher offset xs
    where
      isChar :: Char -> Bool
      isChar x = x >= 'a' && x <= 'z' || x >= 'A' && x <= 'Z'
