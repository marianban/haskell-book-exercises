  -- 1. data Bool = False | True no
  -- 2. data BoolAndSomethingElse a = False' a | True' a yes
  -- 3. data BoolAndMaybeSomethingElse a = Falsish | Truish a yes
  -- 4. newtype Mu f = InF { outF :: f (Mu f) } yes
  -- 5. data D = D (Array Word Word) Int Int no

-- Rearrange the arguments to the type constructor of the datatype
-- so the Functor instance works.
{-# LANGUAGE FlexibleInstances #-}

-- 1.
data Sum a b = First b | Second a

instance Functor (Sum a) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a b c = DeepBlue a b | Something c

instance Functor (Company e e') where
  fmap f (Something c) = Something (f c)
  fmap _ (DeepBlue a b) = DeepBlue a b

-- 3.
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2.
data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.
{-# LANGUAGE FlexibleInstances #-}

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- newtype K a b = K a

-- should remind you of an
-- instance you've written before
{-# LANGUAGE FlexibleInstances #-}
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

-- 4.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9.
-- You’ll need to use recursion.
data List a = Nil | Cons a (List a) deriving (Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

-- 10.
-- A tree of goats forms a Goat-Lord, fearsome poly-creature.

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats ga' ga'' ga''') = MoreGoats (fmap f ga') (fmap f ga'') (fmap f ga''')

-- 11.
{-
You’ll use an extra functor for this one, although your solution
might do it monomorphically without using fmap. 3 Keep in
mind that you will probably not be able to validate this one in
the usual manner. Do your best to make it work.
-}
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read fa) = Read (fmap f fa)
