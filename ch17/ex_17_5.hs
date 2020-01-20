module Ex17_5 where

import Control.Applicative
import Data.List (elemIndex)

-----------
added :: Maybe Integer
added = (+3) <$> (lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6])
-----------

-----------
y :: Maybe Integer
y = lookup (3 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup (2 :: Integer) $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z
-----------

-----------
x1 :: Maybe Int
x1 = elemIndex (3 :: Integer) [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex (4 :: Integer) [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1
-----------

-----------
xs = [1, 2, 3] :: [Integer]
ys = [4, 5, 6] :: [Integer]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x2 <*> y2)
-----------

-----------
newtype Identity a =
    Identity a
    deriving (Eq, Ord, Show)
    
instance Functor Identity where
    fmap f (Identity a) = Identity $ f a
    
instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity x) = Identity $ f x
-----------

-----------
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)
    
instance Functor (Constant a) where
    fmap f (Constant {getConstant = x}) = Constant {getConstant = x}

instance Monoid a => Applicative (Constant a) where 
    pure _ = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant $ mappend x y
-----------