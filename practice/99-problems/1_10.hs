myLast :: [a] -> a
myLast xs = case xs of
    []     -> error "Invalid Arg"
    (x:[]) -> x
    (x:xs) -> myLast xs

myButLast :: [a] -> a
myButLast []        = error "Invalid Arg"
myButLast (x:[])    = error "Invalid Arg"
myButLast (x:y:[])  = y
myButLast (x:xs)    = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _     = error "Invalid Arg"
elementAt (x:_) 1  = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Integer
myLength = foldr (\x -> (+1)) 0

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (==) xs $ myReverse xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ fmap flatten xs

compress :: (Eq a) => [a] -> [a]
compress = compressHelper Nothing

compressHelper :: (Eq a) => Maybe a -> [a] -> [a]
compressHelper _ []           = []
compressHelper Nothing (x:xs) = x : compressHelper (Just x) xs
compressHelper (Just k) (x:xs)
    | x == k    = compressHelper (Just x) xs
    | otherwise = x : compressHelper (Just x) xs


pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = ([x] ++ takeWhile (== x) xs) : (pack $ dropWhile (== x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = (fmap (\x -> (length x, x !! 0))) . pack