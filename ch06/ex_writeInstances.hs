module Ex where

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn i1) (TisAn i2) =
        i1 == i2

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two i1 j1) (Two i2 j2) =
        (i1 == i2) && (j1 == j2)

data StringOrInt = 
    TisAnInt Int
    | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt i1) (TisAnInt i2)        = i1 == i2
    (==) (TisAString s1) (TisAString s2)    = s1 == s2
    (==) _ _                                = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair b1 b2) =
        (a1 == b1) && (a2 == b2)

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 a2) (Tuple b1 b2) =
        (a1 == b1) && (a2 == b2)

data EitherOr a b = 
    Hello a
    | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a1) (Hello a2)      = a1 == a2
    (==) (Goodbye a1) (Goodbye a2)  = a1 == a2
    (==) _ _                        = False