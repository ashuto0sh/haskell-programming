module Identity where
import Test.QuickCheck
import Data.Monoid

data Two a b =
    Two a b
    deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x x1) <> (Two y y1) = Two (x <> y) (x1 <> y1) 
    
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m  = mempty <> m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m  = m <> mempty == m

type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a  b) -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ Two k l

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: (TwoAssoc (Sum Int) (Sum Int)))
    quickCheck (monoidLeftIdentity :: (Two (Sum Int) (Sum Int)) -> Bool)
    quickCheck (monoidRightIdentity :: (Two (Sum Int) (Sum Int)) -> Bool)