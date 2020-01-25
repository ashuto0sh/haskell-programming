module Monads where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-----------------------------------
data Nope a =
    NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
    NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg
    
instance Eq a => EqProp (Nope a) where
    (=-=) = eq
-----------------------------------


-----------------------------------
data BahEither b a =
    PLeft a
    | PRight b
    deriving (Eq, Show)

instance Functor (BahEither a) where
    fmap f (PLeft x) = PLeft $ f x
    fmap _ (PRight x) = PRight  x

instance Applicative (BahEither a) where
    pure x = PLeft x

    _ <*> (PRight y)        = PRight y
    (PRight f) <*> _        = PRight f
    (PLeft f) <*> (PLeft x) = PLeft $ f x

instance Monad (BahEither a) where
    return = pure

    (PRight x) >>= _ = PRight x
    (PLeft x) >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
    arbitrary =
        do
            x <- arbitrary
            y <- arbitrary
            oneof [return $ PLeft x, return $ PRight y]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq
-----------------------------------


-----------------------------------
newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x
    
instance Applicative Identity where
    pure = Identity
    
    (Identity f) <*> (Identity x) = Identity $ f x
    
instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x 

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do
            x <- arbitrary
            return $ Identity x

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq
-----------------------------------


-----------------------------------
data List a =
    Nil
    | Cons a (List a)
    deriving (Show, Eq)

instance Semigroup (List a) where
    Nil <> xs = xs
    xs <> Nil = xs
    (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil

    mappend = (<>)

    mconcat = foldr mappend mempty

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Applicative List where
    pure x = Cons x Nil

    Nil <*> x = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = (fmap f xs) <> (fs <*> xs)

instance Monad List where
    return = pure

    Nil >>= _ = Nil
    (Cons x xs) >>= f = f x `mappend` (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq
-----------------------------------


main :: IO ()
main =
    do
        let
            trigger :: List (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        verboseBatch $ monad trigger