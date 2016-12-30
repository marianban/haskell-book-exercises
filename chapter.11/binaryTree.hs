module BinaryTree where
  data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

  insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
  insert' b Leaf = Node Leaf b Leaf
  insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

  mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
  mapTree _ Leaf = Leaf
  mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

  testTree' :: BinaryTree Integer
  testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

  mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

  mapOkay = if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

  preOrder :: BinaryTree a -> [a]
  preOrder Leaf = []
  preOrder (Node left x right) = [x] ++ (preOrder left) ++ (preOrder right)

  inOrder :: BinaryTree a -> [a]
  inOrder Leaf = []
  inOrder (Node left x right) = (preOrder left) ++ [x] ++ (preOrder right)

  postOrder :: BinaryTree a -> [a]
  postOrder Leaf = []
  postOrder (Node left x right) = (preOrder left) ++ (preOrder right) ++ [x]

  testTree :: BinaryTree Integer
  testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

  testPreorder :: IO()
  testPreorder =
    if preOrder testTree == [2,1,3]
      then putStrLn "Preorder fine"
      else putStrLn "Preaorder bad"

  testInOrder :: IO()
  testInOrder =
    if inOrder testTree == [1,2,3]
      then putStrLn "inOrder fine"
      else putStrLn "inOrder bad"

  testPostOrder :: IO()
  testPostOrder =
    if postOrder testTree == [1,3,2]
      then putStrLn "postOrder fine"
      else putStrLn "postOrder bad"

  foldr' :: (a -> b -> b) -> b -> [a] -> b
  foldr' _ acc [] = acc
  foldr' f acc (x : xs) = f x (foldr f acc xs)

  foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
  foldrTree f acc tree = foldr f acc (preOrder tree)

  foldrTree'' :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
  foldrTree'' _ acc Leaf = acc
  foldrTree'' f acc (Node left x right) = f x (foldrTree'' f acc left) (foldrTree'' f acc right)

  main :: IO()
  main = do
    testPreorder
    testInOrder
    testPostOrder
