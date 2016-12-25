module MyElem where
  myElem :: Eq a => a -> [a] -> Bool
  myElem x = foldr (\x' acc' -> acc' || x' == x) False
  myElem2 :: Eq a => a -> [a] -> Bool
  myElem2 x = any (x==)
