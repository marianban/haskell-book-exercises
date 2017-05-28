module QuickCheckingFunctor where
  import Test.QuickCheck

  functorIndentity :: (Functor f, Eq (f a)) => f a -> Bool
  functorIndentity f = fmap id f == f

  functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
  functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

  main = do
    quickCheck $ \x -> functorIndentity (x :: [Int])
    quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
