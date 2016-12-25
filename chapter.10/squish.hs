module Squish where
  squish :: [[a]] -> [a]
  squish = foldr f [] where
    f :: [a] -> [a] -> [a]
    f xs acc = foldr g acc xs where
      g :: a -> [a] -> [a]
      g x acc = x : acc

  -- squish xs = foldr (\x acc -> (foldr (\x' acc' -> x' : acc') acc x)) [] xs
