module MyFoldL where
  myFoldl :: (b -> a -> b) -> b -> [a] -> b
  myFoldl _ acc [] = acc
  myFoldl f acc (x : xs) = myFoldl f (f acc x) xs
