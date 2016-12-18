module Squish where
  squish :: [[a]] -> [a]
  squish [] = []
  squish (x : xs) = x ++ squish xs
