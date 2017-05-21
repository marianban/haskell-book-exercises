module Main where

import Lib
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, oneof, elements)
import Data.Semigroup (Semigroup, (<>), Sum(..))

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
    elements [(BoolConj x), (BoolConj x)]

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj x') = BoolConj (x && x')
  --(BoolConj True) <> (BoolConj True) = (BoolConj True)
  --_ <> _ = (BoolConj False)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- exercise 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    elements [(BoolDisj x), (BoolDisj x)]

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj x') = BoolDisj (x || x')
  --(BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
  --_ <> _ = (BoolDisj True)

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

newtype Combine a b = Combine { unCombine :: (a -> b) } deriving (Show)

instance Show (a -> b) where show a = "funcion"
-- data Sum = Sum { getSum :: Int } deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
  (Combine { unCombine=u }) <> (Combine { unCombine=u' }) = Combine (u <> u')

-- exercise 10

newtype Comp a = Comp { unComp :: (a -> a) } deriving (Show)

-- instance Show (a -> a) where show a = "function"

instance (Semigroup a) => Semigroup (Comp a) where
  Comp { unComp=u } <> Comp { unComp=u' } = Comp (u<>u')

-- exercise 11

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (Success a), return (Failure b)]

instance (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  (<>) (Success x) (Success x') = Success (x <> x')
  (<>) (Failure x) (Failure x') = Failure (x <> x')
  (<>) (Failure x) _ = Failure x
  (<>) _ (Failure x') = Failure x'

type ValidationType = Validation String String
type ValidationAssoc = ValidationType -> ValidationType -> ValidationType -> Bool

-- exercise 12

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (<>) _ (AccumulateRight (Failure x)) = AccumulateRight (Failure x)
  (<>) _ (AccumulateRight (Success x)) = AccumulateRight (Success x)

instance (Arbitrary a, Arbitrary b) => Arbitrary(AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (AccumulateRight(Success a)), return (AccumulateRight(Failure b))]

type AccumulateRightType = AccumulateRight String String
type AccumulateRightAssoc = AccumulateRightType -> AccumulateRightType -> AccumulateRightType -> Bool

-- exercise 13

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (<>) (AccumulateBoth (Success x)) (AccumulateBoth (Success x')) = AccumulateBoth (Success (x <> x'))
  (<>) (AccumulateBoth (Failure x)) (AccumulateBoth (Failure x')) = AccumulateBoth (Failure (x <> x'))
  (<>) (AccumulateBoth (Success x)) _ = AccumulateBoth (Success x)
  (<>) _ (AccumulateBoth (Success x)) = AccumulateBoth (Success x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return (AccumulateBoth(Success a)), return (AccumulateBoth(Failure b))]

type AccumulateBothType = AccumulateBoth String String
type AccumulateBothAssoc = AccumulateBothType -> AccumulateBothType -> AccumulateBothType -> Bool

-- exercise 1

-- data Trivial = Trivial deriving (Eq, Show)

-- instance Semigroup Trivial where
--   (<>) = undefined

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

monoidLeftIndentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIndentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

-- exercise 2

-- newtype Identity a = Identity a deriving (Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity (x `mappend` y)

-- exercise 3

-- data Two a b = Two a b deriving Show

instance (Semigroup a, Monoid a, Monoid b, Semigroup b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

-- exercise 4

-- newtype BoolConj = BoolConj Bool

instance Monoid BoolConj where
  mempty = mempty True
  mappend = (<>)

-- exercise 5

instance Monoid BoolDisj where
  mempty = mempty False
  mappend = (<>)

-- exercise 6
--
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--  arbitrary = do
--    a <- arbitrary
--    b <- arbitrary
--    return (Combine a b)

instance (Semigroup a, Monoid a, Monoid b, Semigroup b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- exercise 7
instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

-- exercise 8
newtype Mem s a = Mem {
  runMem :: s -> (a, s)
}

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem { runMem=f }) (Mem { runMem=g }) = Mem (\x -> let (a, b) = g x
                                                                 (c, d) = g x
                                                             in (a <> c, d))

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
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  quickCheck (monoidLeftIndentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIndentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidLeftIndentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  --quickCheck (monoidLeftIndentity :: BoolConj -> Bool)
  --quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  --quickCheck (monoidLeftIndentity :: BoolDisj -> Bool)
  --quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  --quickCheck (monoidLeftIndentity :: Combine String String -> Bool)
