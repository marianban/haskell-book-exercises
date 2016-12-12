module AvgGuard where
  avgGuard :: (Fractional a, Ord a) => a -> Char
  avgGuard x
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.7 = 'C'
    | y >= 0.6 = 'D'
    | otherwise = 'F'
    where y = x / 100
