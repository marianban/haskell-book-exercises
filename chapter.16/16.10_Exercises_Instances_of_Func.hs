module InstancesOfFunc where
  import Test.QuickCheck
  import Test.QuickCheck.Function

  -- 1.
  newtype Identity a = Identity a deriving (Eq, Show)

  instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      a <- arbitrary
      return (Identity a)

  instance Functor (Identity) where
    fmap f (Identity a) = Identity (f a)

  functorIndentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIndentity f = fmap id f == f

  functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
  functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

  functorCompose' :: (Eq (f c), Functor f) =>
                      f a
                      -> Fun a b
                      -> Fun b c
                      -> Bool

  functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

  -- 2
  data Pair a = Pair a a deriving (Eq, Show)

  instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
      a <- arbitrary
      a' <- arbitrary
      return (Pair a a')

  instance Functor (Pair) where
    fmap f (Pair a a') = Pair (f a) (f a')

  type IntToInt = Fun Int Int
  type IntToIntFc = [Int] -> IntToInt -> IntToInt -> Bool

  -- 3
  data Two a b = Two a b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

  instance Functor (Two a) where
    fmap f (Two a b) = (Two a (f b))

  --4
  data Three a b c = Three a b c deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary(Three a b c) where
      arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

  instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

  --5
  data Three' a b = Three' a b b deriving (Eq, Show)

  instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Three' a b b)

  instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

  -- 6 data Four a b c d = Four a b c d easy

  -- 7 data Four' a b = Four' a a a b only the last argument will be modified

  -- 8 data Trivial = Trivial - not posible to implement because has kind of *

  main = do
    quickCheck $ \x -> functorIndentity (x :: Identity String)
    quickCheck $ \x -> functorCompose (++"first") (++"second") (x :: Identity String)
    quickCheck $ \x -> functorIndentity (x :: Pair String)
    quickCheck (functorCompose' :: IntToIntFc)
    quickCheck $ \x -> functorIndentity (x :: Two Int Int)
    quickCheck $ \x -> functorCompose (*2) (*1) (x :: Two Int Int)
    quickCheck $ \x -> functorIndentity (x :: Three Int Int String)
    quickCheck $ \x -> functorIndentity (x :: Three' String Int)
    quickCheck $ \x -> functorCompose (*2) (*4) (x :: Three' String Int)
