module Factorial where
  factorials :: [Integer]
  --factorials = scanl (*) 1 [1..]
  factorials = scanl (*) 1 (list 0) where
    list x = (x + 1) : list (x + 1)
