module MyZipList where

--import Test.QuickCheck
import Test.QuickCheck.Checkers
--import Test.QuickCheck.Classes

newtype ZipList' a =
    ZipList' [a]
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take 3000 l
              ys' = let (ZipList' l) = ys
                    in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs
    
append' :: ZipList' z -> ZipList' z -> ZipList' z
append' k (ZipList' []) = k
append' (ZipList' []) k = k
append' (ZipList' xs) (ZipList' ys) = ZipList' $ xs ++ ys

instance Applicative ZipList' where
    pure x = ZipList'$ repeat x
    (<*>) _ (ZipList' []) = ZipList' []
    (<*>) (ZipList' []) _ = ZipList' []
    (<*>) (ZipList' (f:fs)) (ZipList' (x:xs)) = append' (ZipList' [f x])  ((ZipList' fs) <*> (ZipList' xs))
