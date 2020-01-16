module MyOptional where

data MyOptional a =
    Nada
    | Only a
    deriving (Eq, Show)

instance (Semigroup a) => Semigroup (MyOptional a) where
    (<>) (Only x) (Only y) = Only $ x <> y
    (<>) _ _               = Nada

instance (Monoid a) => Monoid (MyOptional a) where
    mempty = Nada
    mappend = (<>)