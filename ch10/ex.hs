module FoldEx where

-- All exercises in this module are impleted using folds

sum' :: [Integer] -> Integer
sum' = foldr (+) 0

length' :: [a] -> Integer
length' = foldr (\_ y -> y+1) 0

prod' :: [Integer] -> Integer
prod' = foldr (*) 1

concat' :: [[a]] -> [a]
concat' = foldr (\x y -> x ++ y) []

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

ex11 :: String -> String -> [String]
ex11 xs ys = [[a,b,c] | a <- xs, b <- ys, c <- xs]

ex11_1 :: String -> String -> [String]
ex11_1 xs ys = [['p',b,c] | b <- ys, c <- xs]

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a b -> (||) (x == a) b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myOr . map (== x)

myReverse :: [a] -> [a]
myReverse = foldl (\xs x -> x : xs) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x : xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (\x xs -> x ++ xs) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x m -> if f x m  == GT then x else m) (head xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x m -> if f x m  == LT then x else m) (head xs) xs