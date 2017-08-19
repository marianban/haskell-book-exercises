import Data.Monoid
import Data.Foldable

-- Write Foldable instances for the following datatypes.

-- 1. data Constant a b = Constant a
data Constant a b = Constant a
instance Foldable (Constant a) where
  foldMap _ _ = mempty

--  2. data Two a b = Two a b
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ y) = f y z

-- 3. data Three a b c = Three a b c
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ z') = f z' z

--  4. data Three' a b = Three' a b b
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

-- 5. data Four' a b = Four' a b b b
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

-- Thinking cap time. Write a filter function for Foldable types using foldMap.

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldMap fb xs where
  fb x = if f x
    then mempty
    else pure x
