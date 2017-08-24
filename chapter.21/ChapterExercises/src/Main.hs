module Main where

import Data.Monoid
import Data.Traversable
import Data.Foldable
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
-- Traversable instances
-- Identity

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> (f a)

instance Arbitrary a => Arbitrary(Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

-- Maybe

data Optional a = Nada | Yep a deriving (Eq, Show, Ord)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [return $ Yep a, return Nada]

instance (Eq a) => EqProp (Optional a) where (=-=) = eq

-- List

data List a = Nil | Cons a (List a) deriving (Eq, Show, Ord)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- instance Monoid (List a) where
--   mempty = Nil
--   mappend Nil ys = ys
--   mappend xs Nil = xs
--   mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil), (10, return (Cons x y))]

instance (Eq a) => EqProp (List a) where (=-=) = eq

-- Three

data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Pair

data Pair a b = Pair a b deriving (Eq, Show, Ord)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = fmap (Pair a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

-- Big

data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big a b b') = (f b) <> (f b')

instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> (f b) <*> (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where (=-=) = eq

-- S

main :: IO ()
main = do
  let trigger = undefined :: Identity (Int, Int, [Int])
  quickBatch (traversable trigger)
  let trigger2 = undefined :: Constant Int (Int, Int, [Int])
  quickBatch (traversable trigger2)
  let trigger3 = undefined :: Optional (Int, Int, [Int])
  quickBatch (traversable trigger3)
  let trigger4 = undefined :: List (Int, Int, [Int])
  quickBatch (traversable trigger4)
  let trigger5 = undefined :: Three Int Int (Int, Int, [Int])
  quickBatch (traversable trigger5)
  let trigger6 = undefined :: Pair Int (Int, Int, [Int])
  quickBatch (traversable trigger6)
  let trigger7 = undefined :: Big Int (Int, Int, [Int])
  quickBatch (traversable trigger7)
