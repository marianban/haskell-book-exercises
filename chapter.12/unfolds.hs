module Unfolds where
  {-
  While the idea of catamorphisms is still relatively fresh in our minds,
  let’s turn our attention to their dual: anamorphisms. If folds, or cata-
  morphisms, let us break data structures down then unfolds let us
  build them up. There are, just as with folds, a few different ways to
  unfold a data structure. We can use them to create finite and infinite
  data structures alike.

  -- iterate is like a very limited
  -- unfold that never ends
  Prelude> :t iterate
  iterate :: (a -> a) -> a -> [a]

  -- because it never ends, we must use
  -- take to get a finite list
  Prelude> take 10 $ iterate (+1) 0
  [0,1,2,3,4,5,6,7,8,9]

  -- unfoldr is more general, the full monty as it were
  Prelude> :t unfoldr
  unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

  -- Using unfoldr to do the same thing as iterate
  Prelude> take 10 $ unfoldr (\b -> Just (b, b+1)) 0
  [0,1,2,3,4,5,6,7,8,9]
  -}

  {-
  1. Write the function myIterate using direct recursion. Compare
  the behavior with the built-in iterate to gauge correctness. Do
  not look at the source or any examples of iterate so that you
  are forced to do this yourself.
  -}

  myIterate :: (a -> a) -> a -> [a]
  myIterate f x = x : myIterate f (f x)

  {-
  2. Write the function myUnfoldr using direct recursion. Compare
  with the built-in unfoldr to check your implementation. Again,
  don’t look at implementations of unfoldr so that you figure it
  out yourself.
  -}
  myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
  myUnfoldr f x = g (f x) [] where
    g (Just y) xs = (fst y) : (g (f (snd y)) xs)
    g Nothing xs = xs

  {-
  3. Rewrite myIterate into betterIterate using myUnfoldr. A hint —
  we used unfoldr to produce the same results as iterate earlier.
  Do this with different functions and see if you can abstract the
  structure out.
  -}

  betterIterate :: (a -> a) -> a -> [a]
  betterIterate f x = myUnfoldr (\x' -> Just (x', f x')) x
