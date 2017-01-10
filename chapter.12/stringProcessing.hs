module StringProcessing where
  {-
  1. Write a recursive function named replaceThe which takes a text/string,
  breaks it into words and replaces each instance of “the” with “a”.
  It’s intended only to replace exactly the word “the”. notThe is a
  suggested helper function for accomplishing this.
  -}
  notThe :: String -> Maybe String
  notThe x
    | x == "the" = Nothing
    | otherwise = Just x

  replaceThe :: String -> String
  replaceThe x = foldr (\x acc -> x ++ " " ++ acc) "" $ map f $ map notThe $ words x where
    f Nothing = "a"
    f (Just x) = x

  {-
  2. Write a recursive function that takes a text/string, breaks it into
  words, and counts the number of instances of ”the” followed by
  a vowel-initial word.
  -}

  isVowel x = elem x "aeiou"

  countTheBeforeVowel :: String -> Integer
  countTheBeforeVowel x = countThe 0 $ words x where
    countThe count [] = count
    countThe count (x : xs)
      | x == "the" = countThe getCount xs
      | otherwise = countThe count xs
      where
        firstChar = head . head
        getCount = case (isVowel $ firstChar xs) of
          True -> (count + 1)
          False -> count

  {-
  3. Return the number of letters that are vowels in a word.
  Hint: it’s helpful to break this into steps. Add any helper func-
  tions necessary to achieve your objectives.
  -}

  countVowels :: String -> Integer
  countVowels xs = toInteger $ length $ filter isVowel xs
