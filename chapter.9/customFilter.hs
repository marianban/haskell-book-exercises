module CustomFilter where
  customFilter :: (a -> Bool) -> [a] -> [a]
  customFilter _ [] = []
  customFilter pred (x : xs)
    | pred x = x : customFilter pred xs
    | otherwise = customFilter pred xs
