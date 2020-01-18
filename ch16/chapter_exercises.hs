{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import Test.QuickCheck
import GHC.Arr

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-----------
data BoolAndSomethingElse a =
    False' a
    | True' a
    deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
    fmap f (False' x) = False' $ f x
    fmap f (True' y) = False' $ f y

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
    arbitrary =
        do
            k <- arbitrary
            oneof [return $ True' k, return $ False' k]
-----------

-----------
data BoolAndMaybeSomethingElse a =
    Falsish
    | Truish a
    deriving (Show, Eq)

instance Functor BoolAndMaybeSomethingElse where
    fmap f (Truish x) = Truish $ f x
    fmap _ Falsish    = Falsish
    
instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary =
        do
            k <- arbitrary
            oneof [return $ Truish k, return Falsish]
-----------

-----------
newtype Mu f =
    InF { outF :: f (Mu f) }
    --deriving (Show, Eq)
-----------

-----------
data D =
    D (Array Word Word) Int Int
-----------

-----------
data Sum a b =
    First b
    | Second a
    deriving (Show, Eq)
    
instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b
-----------

-----------
data Company a c b =
    DeepBlue a c
    | Something b
    
instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c
-----------

-----------
data More b a = 
    L a b a
    | R b a b
    deriving (Eq, Show)
    
instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'
-----------

-----------
data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Show, Eq)

instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ (Desk x)  = Desk x
    fmap _ Finance   = Finance

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            oneof [return Finance, return $ Desk k, return $ Bloor l]
-----------

-----------
data K a b =
    K a
    deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K a) = K a

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary =
        do
            k <- arbitrary
            return $ K k
-----------

-----------
newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

newtype K1 a b =
    K1 a
    deriving (Show, Eq)

instance Functor (Flip K1 a) where
    fmap f (Flip (K1 a)) = Flip (K1 (f a))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K1 a b) where
    arbitrary =
        do
            k <- arbitrary
            return $ Flip (K1 k)
-----------

-----------
data EvilGoateeConst a b =
    GoatyConst b
    deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary =
        do
            k <- arbitrary
            return $ GoatyConst k
-----------

-----------
data LiftItOut f a =
    LiftItOut (f a)
    deriving (Show, Eq)

instance (Functor f) => Functor (LiftItOut f) where
    fmap g (LiftItOut f) = LiftItOut $ fmap g f

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
    arbitrary =
        do
            k <- arbitrary
            return $ LiftItOut k
-----------

-----------
data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ DaWrappa k l
-----------

-----------
data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Show, Eq)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ IgnoringSomething k l
-----------

-----------
data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            return $ Notorious k l m
-----------

-----------
data List a =
    Nil
    | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            oneof [return Nil, return $ Cons k l]
-----------

-----------
data GoatLord a =
    NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving (Eq, Show)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat $ f x
    fmap f (MoreGoats gs1 gs2 gs3) = MoreGoats (fmap f gs1) (fmap f gs2) (fmap f gs3)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            m <- arbitrary
            n <- arbitrary
            oneof [return NoGoat, return $ OneGoat k, return $ MoreGoats l m n]
-----------

-----------
data TalkToMe a =
    Halt
    | Print String a
    | Read (String -> a)
    --deriving (Eq, Show)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s x) = Print s (f x)
    fmap f (Read sa) = Read (fmap f sa)
-----------


main :: IO ()
main = do
         quickCheck (\x -> functorCompose (+1) (*2) (x :: Quant Int Int))
         quickCheck (\x -> functorIdentity (x :: Quant Int Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: K Int Int))
         quickCheck (\x -> functorIdentity (x :: K Int Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: Flip K1 Int Int))
         quickCheck (\x -> functorIdentity (x :: Flip K1 Int Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: EvilGoateeConst Int Int))
         quickCheck (\x -> functorIdentity (x :: EvilGoateeConst Int Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: LiftItOut Maybe Int))
         quickCheck (\x -> functorIdentity (x :: LiftItOut Maybe Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: Parappa Maybe Maybe Int))
         quickCheck (\x -> functorIdentity (x :: Parappa Maybe Maybe Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: IgnoreOne Maybe Maybe Int Int))
         quickCheck (\x -> functorIdentity (x :: IgnoreOne Maybe Maybe Int Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: List Int))
         quickCheck (\x -> functorIdentity (x :: List Int))
         quickCheck (\x -> functorCompose (+1) (*2) (x :: GoatLord Int))
         quickCheck (\x -> functorIdentity (x :: GoatLord Int))