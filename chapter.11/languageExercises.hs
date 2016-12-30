module LanguageExercises where
  import Data.Char

  capitalizeWord :: String -> String
  capitalizeWord (x : xs) = (toUpper x) : xs

  paragraphs :: String -> [String]
  paragraphs [] = []
  paragraphs xs = take ((length $ takeWhile (/='.') xs) + 2) xs : paragraphs (drop 2 $ dropWhile (/='.') xs)

  capitalizeParagraph :: String -> String
  capitalizeParagraph xs = foldr (\x acc -> (capitalizeWord x) ++ acc) [] (paragraphs xs)
