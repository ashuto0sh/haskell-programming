applyTimes :: (Num a, Eq a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = applyTimes (n-1) f $ f b


fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

mull :: Integral a => a -> a -> a
mull a b = helper 0 a b
    where
        helper :: Integral a => a -> a -> a -> a
        helper s _ 0 = s
        helper s a n = helper (s+a) a (n-1)

summ :: Integer -> Integer
summ a = helper 0 a
    where
        helper :: Integer -> Integer ->  Integer
        helper ss 0 = ss
        helper ss n = helper (ss+n) (n-1)


mcc :: Integer -> Integer
mcc n
    | n > 100   = n-10
    | otherwise = mcc $ mcc $ n+11