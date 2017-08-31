{-# LANGUAGE InstanceSigs #-}
module ReadingComprehension where

import Control.Applicative

-- 1.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2.

newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

-- 3.
{-
Implement the Applicative for Reader.
To write the Applicative instance for Reader, we’ll use an exten-
sion called InstanceSigs. It’s an extension we need in order to
assert a type for the typeclass methods. You ordinarily cannot
assert type signatures in instances. The compiler already knows
the type of the functions, so it’s not usually necessary to assert
the types in instances anyway. We did this for the sake of clarity,
to make the Reader type explicit in our signatures.
-}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- Exercise: Reader Monad
-- 1. Implement the Reader Monad.

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) $ r

-- getDogR :: Reader Person Dog
-- getDogR = Reader $ liftM2 Dog dogName address
