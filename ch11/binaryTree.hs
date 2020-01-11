module BinaryTree where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show, Ord)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left x1 right)
    | x < x1    = Node (insert' x left) x1 right
    | x > x1    = Node left x1 (insert' x right)
    | otherwise = Node left x1 right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
    Node (mapTree f left) (f x) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left x right) =
    [x] ++ preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left x right) =
    inOrder left ++ [x] ++ inOrder right
    
postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left x right) =
    postOrder left ++ postOrder right ++ [x]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = if preOrder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder = if inOrder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder = if postOrder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder

foldTree :: (a -> b -> b)
    -> b
    -> BinaryTree a
    -> b
foldTree f b = foldr f b . preOrder