data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right 
  | b > a  = Node left a (insert' b right)

map' :: (a -> b) -> BinaryTree a -> BinaryTree b
map' _ Leaf = Leaf
map' f (Node l n r) = Node (map' f l) (f n) (map' f r)   

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if map' (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"


testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left value right) = value : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left value right) = preorder left ++ [value] ++ preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left value right) = preorder left ++ preorder right ++ [value]

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "postorder fine!"
  else putStrLn "Bad news bears."


foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b right) left)


foldTree insert' leaf testTree == testTree


