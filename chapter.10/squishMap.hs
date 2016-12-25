module SquishMap where
  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap f = foldr (\x acc -> (f x) ++ acc) []
  squishAgain :: [[a]] -> [a]
  squishAgain = squishMap id
