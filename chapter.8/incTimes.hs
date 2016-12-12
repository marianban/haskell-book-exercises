module IncTimes where
  incTimes :: (Eq a, Num a) => a -> a -> a
  incTimes 0 n = n
  incTimes times n = 1 + (incTimes (times - 1) n)

  applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
  applyTimes 0 f b = b
  applyTimes times f b = f . applyTimes (times - 1) f $ b

  incTimes' :: (Eq a, Num a) => a -> a -> a
  incTimes' times n = applyTimes times (+1) n
