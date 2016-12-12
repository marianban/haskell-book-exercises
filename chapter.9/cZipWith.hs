module CZipWith where
  cZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
  cZipWith _ [] _ = []
  cZipWith _ _ [] = []
  cZipWith f (x : xs) (y : ys) = (f x y) : cZipWith f xs ys
