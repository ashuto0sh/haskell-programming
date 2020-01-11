{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module TooMany where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42 

newtype Goats =
    Goats Int
    deriving (Eq, Show, TooMany)


newtype MyPair =
    MyPair (Int, String)
    deriving (Eq, Show)

instance TooMany MyPair where
    tooMany (MyPair (x, _)) =  x > 47

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (n1, n2) = tooMany n1 || tooMany n2