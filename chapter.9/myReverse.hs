module MyReverse where
  myReverse :: [a] -> [a]
  myReverse [] = []
  myReverse (x : xs) = myReverse xs ++ [x]
