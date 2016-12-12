module DividedBy where
  dividedBy :: Integral a => a -> a -> Maybe (a, a)
  dividedBy divident divisor = go (abs divident) (abs divisor) 0
    where
      go divid divis count
        | divis == 0 = Nothing
        | divid < divis && ((divident < 0 && divisor > 0) || (divident > 0 || divisor < 0)) = Just (-count, -divid)
        | divid < divis = Just (-count, -divid)
        | otherwise = go (divid - divis) divis (count + 1)
