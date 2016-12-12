module MultiplieadBy where
  multipliedBy :: Integral a => a -> a -> a
  multipliedBy multiplicand multiplier
    | multiplier == 0 = 0
    | otherwise = multiplicand + multipliedBy multiplicand (multiplier - 1)
