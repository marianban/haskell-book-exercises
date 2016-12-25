module MyMaximumBy where
  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy f (x : xs) = foldr g x xs where
    g x x'
      | ((f x x') == GT) = x
      | otherwise = x'

  -- myMaximumBy f (x : xs) = foldr (\x' x'' -> if ((f x' x'') == GT) then x' else x'') x xs
