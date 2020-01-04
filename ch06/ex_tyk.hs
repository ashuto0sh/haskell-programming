module Tk where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk a2b a b = (a2b a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith a2b x a = (a2b a) + (fromIntegral x)