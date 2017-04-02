module ValidatingAssociativity where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type S = String
type B = Bool

main :: IO ()
main = quickCheck (monoidAssoc :: S -> S -> S -> B)
