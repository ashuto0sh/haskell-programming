module Code where

tensDigit :: Integral a => a -> a
tensDigit x = snd . flip divMod 10 $ fst . divMod x $ 10

foldBool :: a -> a -> Bool -> a
foldBool a b o = case o of
    True  -> a
    False -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b o
    | o == True = a
    | otherwise = b

g :: (a -> b) -> (a, c) -> (b, c)
g a2b (x, z) = (a2b x, z)