module Main where

import Lib
import Data.Monoid
import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- 1. Welcome to the Nope Monad, where nothing happens and nobody cares.

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq

-- 2.
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  _ <*> (Right' b) = Right' b
  (Right' b) <*> _ = Right' b
  (Left' f) <*> (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

-- 3. Write a Monad instance for Identity.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

{-
4. This one should be easier than the Applicative instance was.
Remember to use the Functor that Monad requires, then see
where the chips fall.
-}
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  Nil `mappend` Nil = Nil
  Nil `mappend` ys = ys
  xs `mappend` Nil = xs
  (Cons x xs) `mappend` ys = Cons x $ xs `mappend` ys

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs = fmap f xs <> (fs <*> xs)

instance Monad List where
  return = pure
  (Cons x xs) >>= f = -- f x <> (xs >>= f)
    let (Cons x' _) = f x
    in Cons x' (xs >>= f)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil), (10, return $ Cons x y)]

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x $ take' (n - 1) xs

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' = take' 3000 xs
      ys' = take' 3000 ys

{-
Write the following functions using the methods provided by
Monad and Functor. Using stuff like identity and composition is fine,
but it has to typecheck with types provided.
-}

-- 1.
j :: Monad m => m (m a) -> m a
j = join

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = liftM

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

-- 6.

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

main :: IO ()
main = do
  let trigger = undefined :: Nope (Int, String, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
  let trigger2 = undefined :: PhhhbbtttEither (Int, String, Int) (Int, String, Int)
  quickBatch $ functor trigger2
  quickBatch $ applicative trigger2
  quickBatch $ monad trigger2
  let trigger3 = undefined :: Identity (Int, String, Int)
  quickBatch $ functor trigger3
  quickBatch $ applicative trigger3
  quickBatch $ monad trigger3
  let trigger4 = undefined :: List (Int, String, Int)
  quickBatch $ functor trigger4
  quickBatch $ applicative trigger4
