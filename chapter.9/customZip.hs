module CustomZip where
  customZip :: [a] -> [b] -> [(a, b)]
  customZip [] _ = []
  customZip _ [] = []
  customZip (x : xs) (y : ys) = (x, y) : customZip xs ys
