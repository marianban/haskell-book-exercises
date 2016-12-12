module Trivial1 where
  data Trivial = Trivial'
  instance Eq Trivial where
    Trivial' == Trivial' = True -- same as (==) Trivial' Trivial' = True
