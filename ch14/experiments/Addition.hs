module Addition where
    
import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom =
    go num denom 0
    where
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Num a, Eq a) => a -> a -> a
multiplyBy a b =
    helper a b 0
    where
        helper m n r
            | n == 0    = r
            | otherwise = helper m (n-1) (r+a)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do 
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1::Int) `shouldBe` True
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 (5::Int) `shouldBe` (4, 2)
        it "22 multiplied by 7 is 154" $ do
            multiplyBy 22 (7::Int) `shouldBe` 154
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x::Int)
