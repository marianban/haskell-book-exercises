module ValidateTheWord where
  {-
  Use the Maybe type to write a function that counts the number of
  vowels in a string and the number of consonants. If the number
  of vowels exceeds the number of consonants, the function returns
  Nothing. In many human languages, vowels rarely exceed the number
  of consonants so when they do, it indicates the input isn’t a real word
  (that is, a valid input to your dataset):
  -}

  isVowel :: Char -> Bool
  isVowel x = elem x "aeiou"

  countVowels :: String -> Integer
  countVowels xs = toInteger $ length $ filter isVowel xs

  newtype Word' = Word' String deriving (Eq, Show)

  mkWord :: String -> Maybe Word'
  mkWord xs
    | vowelCount > consonantCount = Nothing
    | otherwise = Just (Word' xs)
    where
      vowelCount = countVowels xs
      consonantCount = (toInteger $ length xs) - vowelCount
