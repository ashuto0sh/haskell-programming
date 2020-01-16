import Test.QuickCheck
import Data.Monoid

data Trivial = 
    Trivial
    deriving (Eq, Show)
    
instance Semigroup Trivial where
    _ <> _ = Trivial
    
instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a =
    Identity a
    deriving (Show, Eq)

data Two a b =
    Two a b
    deriving (Show, Eq)

newtype BoolConj =
    BoolConj Bool
    deriving (Eq, Show)

newtype BoolDisj =
    BoolDisj Bool
    deriving (Eq, Show)

data Or a b =
    Fst a
    | Snd b
    deriving (Eq, Show)

newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine a b"

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> (f x) <> (g x))

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _ <> _ = BoolDisj True

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _ <> _ = BoolConj False

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    _ <> (Snd y) = Snd y
    _ <> (Fst y) = Fst y

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x y) <> (Two x1 y1) =
        Two (x <> x1) (y <> y1)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) =
        Identity $ x <> y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            return $ Two k l

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary =
        do
            k <- arbitrary
            return $ Identity k

instance Arbitrary BoolConj where
    arbitrary =
        do
            k <- arbitrary
            return $ BoolConj k

instance Arbitrary BoolDisj where
    arbitrary =
        do
            k <- arbitrary
            return $ BoolDisj k

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary =
        do
            k <- arbitrary
            l <- arbitrary
            oneof [return $ Fst k, return $ Snd l]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary =
        do
            k <- arbitrary
            return $ Combine (\x -> k)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc a = (Identity a) -> (Identity a) -> (Identity a) -> Bool
type TwoAssoc a b = (Two a b) -> (Two a b) -> (Two a b) -> Bool
type BoolAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc a b = (Or a b) -> (Or a b) -> (Or a b) -> Bool
type CombineAssoc a b = a -> (Combine a b) -> (Combine a b) -> (Combine a b) -> Bool

combineSemigroupAssoc :: (Eq b, Show b, Semigroup b)
                      => a
                      -> Combine a b
                      -> Combine a b
                      -> Combine a b
                      -> Bool
combineSemigroupAssoc x (Combine f) (Combine g) (Combine h) =
  (f x <> (g x <> h x)) == ((f x <> g x) <> h x)

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: (IdentityAssoc String))
    quickCheck (semigroupAssoc :: (TwoAssoc String String))
    quickCheck (semigroupAssoc :: (TwoAssoc String String))
    quickCheck (semigroupAssoc :: BoolAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: (OrAssoc String String))
    verboseCheck (combineSemigroupAssoc :: (CombineAssoc (Sum Int) (Sum Int)))

