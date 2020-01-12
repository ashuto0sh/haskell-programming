module BinaryTree where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a =
    case f a of
        Nothing         -> Leaf
        Just (x, y, z)  -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n =
    unfold
        (\x -> if x == n then Nothing else Just (x+1, x, x+1))
        0