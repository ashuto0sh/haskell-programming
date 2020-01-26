
-----------99 questions/1 to 10--------
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
-----------99 questions/1 to 10--------

-----------99 questions/11 to 20--------
data Multiple a =
    Single a
    | Multiple Int a
    deriving (Eq, Show)

convert :: Multiple a -> [a]
convert (Single x) = [x]
convert (Multiple n x) = replicate n x

encode' :: (Eq a) => [a] -> [Multiple a]
encode' = fmap (\(n,x) -> if n==1 then Single x else Multiple n x) . encode

decodeModified :: [Multiple a] -> [a]
decodeModified = concat . fmap convert

encodeDirect :: (Eq a) => [a] -> [Multiple a]
encodeDirect []     = []
encodeDirect (x:xs) = mkMultiple x (takeWhile (== x) xs) : encodeDirect (dropWhile (==x) xs)
    where
        mkMultiple x [] = Single x
        mkMultiple x xs = Multiple (1+length xs) x

repli :: [a] -> Int -> [a]
repli xs n = concat $ fmap (replicate n) xs

dupli :: [a] -> [a]
dupli xs = repli xs 2

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = foldr (\(k, x) xs -> if k`mod`n==0 then xs else x:xs) [] (zip [1..] xs)

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

slice :: [a] -> Int -> Int -> [a]
slice xs s e = take (e-ss) $ drop ss xs
    where ss = s-1

rotate :: [a] -> Int -> [a]
rotate xs n = slice (cycle xs) s e
    where
        s = n+1
        e = (n + length xs)

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (fst r, x : snd r)
    where
        r = removeAt (n-1) xs
-----------99 questions/11 to 20--------