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

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

type S = String
type B = Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRigthIdentity :: Bull -> Bool)
