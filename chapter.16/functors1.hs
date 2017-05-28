module Functors1 where
  data FixMePls = FixMe | Pls deriving (Eq, Show)
  instance Functor FixMePls where
    fmap = error "it doesn't matter, it won't compile"
