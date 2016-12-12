module EnumFromTo where
  eftBool :: Bool -> Bool -> [Bool]
  eftBool False False = [False]
  eftBool False True = [False, True]
  eftBool True True = [True]
  eftBool _ _ = []

  ord :: (Eq a, Ord a, Enum a) => a -> a -> [a]
  ord from to
    | from > to = []
    | from == to = [from]
    | otherwise = from : ord (succ from) to

  eftOrd :: Ordering -> Ordering -> [Ordering]
  eftOrd = ord

  eftInt :: Int -> Int -> [Int]
  eftInt = ord

  eftChar :: Char -> Char -> [Char]
  eftChar = ord
