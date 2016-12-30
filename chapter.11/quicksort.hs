module Quicksort where
  quicksort :: (Ord a, Eq a) => [a] -> [a]
  quicksort [] = []
  quicksort (x : xs) = quicksort [x' | x' <- xs, x' <= x] ++ [x] ++ quicksort [x'' | x'' <- xs, x'' > x]
