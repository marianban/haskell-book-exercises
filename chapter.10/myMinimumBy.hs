module MyMinimumBy where
  myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  myMinimumBy f (x : xs) = foldr g x xs where
    g x' x''
      | ((f x' x'') == LT) = x'
      | otherwise = x''
