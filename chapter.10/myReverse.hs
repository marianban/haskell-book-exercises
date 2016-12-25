module MyReverse where
  myReverse :: [a] -> [a]
  myReverse = foldl (flip (:)) []
