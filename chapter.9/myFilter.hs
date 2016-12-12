module MyFilter where
  myFilter :: String -> [String]
  myFilter xs = filter pred (words xs) where
    pred :: String -> Bool
    pred x
      | elem x ["the", "a", "an"] = False
      | otherwise = True
