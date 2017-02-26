module Lib where

import Data.Char

ordA = ord 'A'
ordZ = ord 'Z'
orda = ord 'a'
ordz = ord 'z'
maxLength = ordz - orda

shiftChar :: Int -> Char -> Char
shiftChar 0 x = x
shiftChar n x = chr $ normalize ((ord x) + n) where
  normalize :: Int -> Int
  normalize x
    | x > ordz = orda + (mod (x - ordz - 1) (maxLength + 1))
    | x < orda = orda + maxLength - (mod (orda - x - 1) (maxLength + 1))
    | otherwise = x

generateKeyword :: [Char] -> [Char] -> [Char]
generateKeyword keyword message = f message 0 where
  f :: [Char] -> Int -> [Char]
  f [] _ = []
  f (x : xs) index
    | isAlpha x = (keyword !! (mod index (length keyword))) : (f xs (index + 1))
    | otherwise = x : (f xs index)

vigenere :: [Char] -> [Char] -> [Char]
vigenere keyword message = map encodeChar (zip message [0..]) where
  encodeChar :: (Char, Int) -> Char
  encodeChar (x, i)
    | isAlpha x = shiftChar ((ord (encodingTable !! i)) - orda) x
    | otherwise = x
    where
      encodingTable :: [Char]
      encodingTable = generateKeyword keyword message

vigenereDecode :: [Char] -> [Char] -> [Char]
vigenereDecode keyword message = map encodeChar (zip message [0..]) where
  encodeChar :: (Char, Int) -> Char
  encodeChar (x, i)
    | isAlpha x = shiftChar (-((ord (encodingTable !! i)) - orda)) x
    | otherwise = x
    where
      encodingTable :: [Char]
      encodingTable = generateKeyword keyword message

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
