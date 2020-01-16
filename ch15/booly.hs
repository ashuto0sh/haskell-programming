module Boolyan where

data Booly a =
    False'
    | True'
    deriving (Eq, Show)

instance Semigroup (Booly a) where
    (<>) True' True' = True'
    (<>) _ _         = False'

instance Monoid (Booly a) where
    mempty = True'