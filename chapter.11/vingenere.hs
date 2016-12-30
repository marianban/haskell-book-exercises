module Vigenere where
  import Data.Char

  ordA = ord 'a'
  ordZ = ord 'z'
  maxLength = ordZ - ordA

  shiftChar :: Int -> Char -> Char
  shiftChar 0 x = x
  shiftChar n x = chr $ normalize ((ord x) + n) where
    normalize :: Int -> Int
    normalize x
      | x > ordZ = ordA + (mod (x - ordZ - 1) (maxLength + 1))
      | x < ordA = ordA + maxLength - (mod (ordA - x - 1) (maxLength + 1))
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
      | isAlpha x = shiftChar ((ord (encodingTable !! i)) - ordA) x
      | otherwise = x
      where
        encodingTable :: [Char]
        encodingTable = generateKeyword keyword message
