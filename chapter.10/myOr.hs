module MyOr where
  myOr :: [Bool] -> Bool
  myOr = foldr (||) False
  myOr2 :: [Bool] -> Bool
  myOr2 = foldr (\a b -> a || b) False
