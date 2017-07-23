module Main where

import Test.QuickCheck.Checkers
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
  -- this is for ZipListApplicative exercise
  (Cons f fx) <*> (Cons x xs) = Cons (f x) (fx <*> xs)
  --(Cons f fx) <*> xs = (fmap f xs) <> (fx <*> xs)

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

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs)
  | n <= 0 = Nil
  | otherwise = (Cons x Nil) `append` (take' (n-1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  (ZipList' fx) <*> (ZipList' xs) = ZipList' $ fx <*> xs

main :: IO ()
main = someFunc
