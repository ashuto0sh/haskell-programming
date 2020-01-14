multiplyBy :: (Num a, Eq a) => a -> a -> a
multiplyBy a b =
    helper a b 0
    where
        helper a b r
            | b == 0    = r
            | otherwise = helper a (b-1) (r+a)