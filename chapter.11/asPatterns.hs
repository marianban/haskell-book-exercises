module AsPatterns where
  import Data.Char

  doubleUp :: [a] -> [a]
  doubleUp [] = []
  doubleUp xs@(x:_) = x : xs

  --1. This should return True if (and only if) all the values in the
  --first list appear in the second list, though they need not be
  --contiguous.
  isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
  isSubsequenceOf xs ys = all (\x -> any (==x) ys) xs

  --2. Split a sentence into words, then tuple each word with the capi-
  --talized form of each.
  capitalizeWords :: String -> [(String, String)]
  capitalizeWords xs = map f (words xs) where
    f :: String -> (String, String)
    f word@(x:xs) = (word, (toUpper x) : xs)
