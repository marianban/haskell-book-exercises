module MyOr where
  myOr :: [Bool] -> Bool
  myOr [] = False
  myOr (x:xs) = x || myOr xs
