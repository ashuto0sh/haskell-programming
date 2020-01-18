module HigherKinded where

data Two a b =
    Two a b
    deriving (Show, Eq)

data Or a b =
    First a
    | Second b
    deriving (Show, Eq)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second (f b)