module Possibly where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

----------------
data Possibly a =
    LolNope
    | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers x) = Yeppers $ f x
    fmap _ LolNope     = LolNope

instance (Arbitrary a) => Arbitrary (Possibly a) where
    arbitrary = 
        do
            k <- arbitrary
            frequency [(1, return LolNope), (1, return (Yeppers k))]
----------------

----------------
data Sum a b =
    First a
    | Second b
    deriving (Show, Eq)

instance Functor (Sum a) where
    fmap _ (First x) = First x
    fmap f (Second y) = Second $ f y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a  b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            frequency [(1, return $ First k), (1, return $ Second l)]
----------------
