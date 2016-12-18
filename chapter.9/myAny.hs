module MyAny where
  myAny :: (a -> Bool) -> [a] -> Bool
  myAny _ [] = False
  myAny f (x : xs) = f x || myAny f xs
