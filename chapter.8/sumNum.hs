module SumNum where
  sumNum :: (Eq a, Num a) => a -> a
  sumNum n
    | n == 0 = 0
    | otherwise = n + sumNum (n - 1)
