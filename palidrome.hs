module Palidrome where
  isPalidrome :: (Eq a) => [a] -> Bool
  isPalidrome x = reverse x == x
