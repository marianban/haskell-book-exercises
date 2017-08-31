module FunctionApplicative where

import Control.Applicative (liftA2)

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address
} deriving (Eq, Show)

data Dog = Dog {
  dogsName :: DogName,
  dogsAddress :: Address
} deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Seasame street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

getDogR'' :: Person -> Dog
getDogR'' = myLiftA2 Dog dogName address 
