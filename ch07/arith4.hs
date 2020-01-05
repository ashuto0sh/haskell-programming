module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main :: IO()
main = do
    print ((roundTrip2 (4::Integer)) :: Integer)
    print (roundTrip 4 :: Integer)
    print (id 4 :: Integer)

