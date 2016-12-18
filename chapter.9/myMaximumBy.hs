module MyMiminumBy where
  myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
  myMinimumBy f (x : xs) = m f xs x where
    m :: (a -> a -> Ordering) -> [a] -> a -> a
    m _ [] y = y
    m f (x : xs) y = m f xs (if (f x y == LT) then x else y)
