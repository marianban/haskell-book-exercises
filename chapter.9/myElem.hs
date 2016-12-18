module MyElem where
  myElem :: Eq a => a -> [a] -> Bool
  myElem _ [] = False
  myElem x' (x : xs) = x' == x || myElem x' xs
