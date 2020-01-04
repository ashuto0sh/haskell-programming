i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x 

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ x = x

r :: [a] -> [a]
r = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c (a2b a)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' a2b x = a2b x