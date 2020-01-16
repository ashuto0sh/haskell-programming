module Identity where
import Test.QuickCheck
import Data.Monoid

newtype Identity a =
    Identity a
    deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y
    
instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m  = mempty <> m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m  = m <> mempty == m

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do
            k <- arbitrary
            return $ Identity k

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: (IdentityAssoc String))
    quickCheck (monoidLeftIdentity :: (Identity String) -> Bool)
    quickCheck (monoidRightIdentity :: (Identity String) -> Bool)