{-# LANGUAGE FlexibleInstances #-}
module NewType where
  class TooMany a where
    tooMany :: a -> Bool

  instance TooMany Int where
    tooMany n = n > 42

  newtype Goats = Goats Int deriving Show

  instance TooMany Goats where
    tooMany (Goats n) = n > 42

  instance TooMany String where
    tooMany s = (length s) > 42

  --instance TooMany (Int, Int) where
  --  tooMany (x, y) = (x + y) > 42

  instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x , y) = tooMany x && tooMany y
