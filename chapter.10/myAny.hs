module MyAny where
  myAny :: (a -> Bool) -> [a] -> Bool
  myAny f = foldr (\a b -> b || f a) False
