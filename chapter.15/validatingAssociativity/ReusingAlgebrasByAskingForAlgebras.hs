module ReusingAlgebras where
  import Data.Monoid
  data Booly a = False' | True' deriving (Eq, Show)
  instance Monoid (Booly a) where
    mappend False' _ = False'
    mappend _ False' = False'
    mappend True' True' = True'
-- Exercise: Optional Monoid
-- Write the Monoid instance for our Maybe type renamed to Optional.
  data Optional a = Nada | Only a deriving(Eq, Show)
  instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only x) (Only y) = Only (x <> y)
    mappend Nada (Only x) = Only x
    mappend (Only x) (Nada) = Only x
    mappend Nada Nada = Nada
