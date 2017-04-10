module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRigthIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRigthIdentity a = (a <> mempty) == a

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only x)) _ = First' (Only x)
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend _ _ = First' Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, fmap Only arbitrary]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = (First' `fmap` arbitrary)

firstMappend :: First' a -> First' a -> First' a
firstMappend = (<>)

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRigthIdentity :: FstId)
