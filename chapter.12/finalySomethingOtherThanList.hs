module SomethingOtherThanList where
  {-
  Given the BinaryTree from last chapter, complete the following exer-
  cises. Here’s that datatype again:
  -}
  data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

  -- 1. Write unfold for BinaryTree.
  unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
  unfold f x = g (f x) where
    g Nothing = Leaf
    g (Just (x', y, x'')) = Node (unfold f x') y (unfold f x'')

  {-
  -- 2. Make a tree builder.
  Using the unfold function you’ve just made for BinaryTree, write
  the following function:
  -}
  treeBuild :: Integer -> BinaryTree Integer
  treeBuild x = unfold f 0 where
    f x'
      | x' == x = Nothing
      | otherwise = Just (x'+1,x',x'+1)
