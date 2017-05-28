module Functors4 where
  data FixMePls a = FixMe | Pls a deriving (Eq, Show)

  instance Functor (FixMePls a) where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)
