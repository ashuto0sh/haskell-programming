module QCFunctors where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f 

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- f = \x -> functorIdentity x
-- c = functorCompose (+1) (*2)

-------------
newtype Identity a =
    Identity a
    deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do
            k <- arbitrary
            return $ Identity k
-------------

-------------
data Pair a =
    Pair a a
    deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ Pair k l
-------------

-------------
data Two a b =
    Two a b
    deriving (Show, Eq)

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ Two k l
-------------

-------------
data Three a b c =
    Three a b c
    deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            return $ Three k l m
-------------

-------------
data Three' a b =
    Three' a b b
    deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            return $ Three' k l m
-------------

-------------
data Four a b c d =
    Four a b c d
    deriving (Show, Eq)

instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z (f w)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            n <- arbitrary
            return $ Four k l m n
-------------

-------------
data Four' a b =
    Four' a a a b
    deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z (f w)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            n <- arbitrary
            return $ Four' k l m n
-------------
