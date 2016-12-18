module SquishMap where
  squishMap :: (a -> [b]) -> [a] -> [b]
  squishMap _ [] = []
  squishMap f (x : xs) = f x ++ squishMap f xs
