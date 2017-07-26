module EitherVersusValidation where

import Data.Monoid
import Control.Applicative

data Validation err a =
  Failure err | Success a
  deriving (Eq, Show)

validToEither :: Valid;ation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
ei;therToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

data Errors =
  DevidedByZero |
  StackOverflow |
  MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure err) = Failure err
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Failure e) <*> (Failure e') = Failure $ e <> e'
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e') = Failure e'
  (Success f) <*> (Success x) = Success $ f x
