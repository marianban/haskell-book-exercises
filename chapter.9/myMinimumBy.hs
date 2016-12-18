module MyMaximumBy where
  myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
  myMaximumBy f (x : xs) = m f xs x where
    m :: (a -> a -> Ordering) -> [a] -> a -> a
    m _ [] y = y
    m f (x : xs) y = m f xs (if (f x y == GT) then x else y)
