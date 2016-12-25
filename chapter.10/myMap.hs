module MyMap where
  myMap :: (a -> b) -> [a] -> [b]
  myMap f = foldr (\x acc -> f x : acc) []
