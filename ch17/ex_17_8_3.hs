import Test.QuickCheck
import Test.QuickCheck.Checkers

data Validation e a =
    Failure' e
    | Success' a
    deriving (Eq, Show)
    
instance Functor (Validation e) where
    fmap f (Success' x) = Success' $ f x
    fmap _ (Failure' k) = (Failure' k)
    
instance Monoid e => Applicative (Validation e) where
    pure x = Success' x
    (<*>) (Failure' x) _ = Failure' x
    (<*>) _ (Failure' x) = Failure' x
    (<*>) (Success' f) (Success' x) = Success' $ f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Success' a, Failure' b]

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq