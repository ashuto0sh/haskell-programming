module BadMonad where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
    CountMe (Sum Int) a
    deriving (Eq, Show)
    
instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe mempty
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)
    
instance Monad CountMe where
    return = pure
    CountMe n a >>= f =
        let CountMe _ b = f a
        in CountMe (n + 1) b
        
instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary
    
instance Eq a => EqProp (CountMe a) where
    (=-=) = eq
    
main =
    do
        let
            trigger :: CountMe (Int, String, Int)
            trigger = undefined
        quickBatch $ functor trigger
        quickBatch $ applicative trigger
        quickBatch $ monad trigger
