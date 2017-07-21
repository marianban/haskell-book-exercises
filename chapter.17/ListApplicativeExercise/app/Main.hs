module Main where

import Control.Applicative
import Data.Monoid
import Lib

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend xs Nil = xs
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fx) <*> xs = (fmap f xs) <> (fx <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append xs Nil = xs
append (Cons x xs) ys = Cons x (xs `append` ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x (fold f b xs)

fold' :: (a -> b -> b) -> b -> List a -> b
fold' _ b Nil = b
fold' f b (Cons x xs) = fold f (f x b) xs

concat' :: List (List a) -> List a
concat' xs = fold append Nil xs

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f xs = concat' $ fmap f xs

main :: IO ()
main = someFunc
