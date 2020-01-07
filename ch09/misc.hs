module MyList where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

enumFromTo' :: (Enum a, Ord a) => a -> a -> [a]
enumFromTo' x y = 
    helper x y []
    where
        helper a b xs 
            | a > b     = xs
            | a == b    = xs ++ [a]
            | otherwise = helper (succ a) y (xs ++ [a])

take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 _      = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x       = x : (takeWhile' f xs)
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f xss@(x:xs)
    | f x       = dropWhile' f xs
    | otherwise = xss

myWords :: String -> [String]
myWords "" = []
myWords (' ':xs) = myWords xs
myWords xs = (takeWhile' (\x -> x /= ' ') xs) : myWords (dropWhile' (\x -> x /= ' ') xs)