module Exercise where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

pr1 = [(x, y) | x <- mySqr, y <- myCube]

pr2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

myfilter :: String -> [String]
myfilter = filter (\x -> ((x /= "a") && (x /= "an") && (x /= "the"))) . words

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (\x -> \y -> (x,y))

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (\x -> x == a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Empty list"
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) =
    case f x y of
        LT -> y
        EQ -> x
        GT -> x
        where
            y = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Empty list"
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) =
    case f x y of
        LT -> x
        EQ -> x
        GT -> y
        where
            y = myMinimumBy f xs