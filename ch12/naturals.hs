module Naturals where

data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (+1) $ natToInteger n

integerToNatHelper :: Integer -> Nat
integerToNatHelper n
    | n == 0    = Zero
    | otherwise = Succ $ integerToNatHelper $ n-1

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just $ integerToNatHelper n