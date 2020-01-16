module MonoidEx where
import Test.QuickCheck
import Data.Monoid

data Trivial =
    Trivial
    deriving (Eq, Show)
    
instance Semigroup Trivial where
    _ <> _ = Trivial
    
instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)
    
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m  = mempty <> m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m  = m <> mempty == m

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
    arbitrary = return Trivial

main :: IO ()
main = 
    do 
        let
            sa = semigroupAssoc 
            mli = monoidLeftIdentity
            mlr = monoidRightIdentity
        quickCheck (sa :: TrivAssoc)
        quickCheck (mli :: Trivial -> Bool)
        quickCheck (mlr :: Trivial -> Bool)