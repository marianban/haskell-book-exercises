module Main where

import Lib
import Test.QuickCheck
import Data.Semigroup

-- exercise 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- exercise 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

--exercise 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

--exercise 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

type ThreeString = Three String String String
type ThreeAsoc = ThreeString -> ThreeString -> ThreeString -> Bool

-- exercise 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
  => Semigroup (Four a b c d) where
    (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

type FourType = Four [Int] [Int] [Int] String
type FourAssoc = FourType -> FourType -> FourType -> Bool

-- exercise 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return (BoolConj x)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = (BoolConj True)
  _ <> _ = (BoolConj False)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- exercise 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return (BoolDisj x)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
  _ <> _ = (BoolDisj True)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- exercise 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Fst a), return (Snd b)]

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
  (Snd x) <> _ = (Snd x)
  _ <> (Snd y) = (Snd y)
  _ <> (Fst x) = (Fst x)

type OrAssoc = Or String [Int] -> Or String [Int] -> Or String [Int] -> Bool

-- exercise 9

newtype Combine a b = Combine { unCombine :: (a -> b) }

data Sum = Sum { getSum :: Int } deriving (Eq, Show)

-- instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAsoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
