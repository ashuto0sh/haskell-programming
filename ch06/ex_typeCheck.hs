import Data.List

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
sigmund _ = myX


--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

mySort :: [Char] -> [Char] 
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

