module Database where

import Data.Time

data DatabaseItem = DbString String 
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)) ,
      DbNumber 9001 ,
      DbNumber 90 ,
      DbNumber 9 ,
      DbNumber 1 ,
      DbString "Hello, world!" ,
      DbDate (UTCTime 
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123)
             )
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where
        f (DbDate d) ys = d : ys
        f _ ys          = ys

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where
        f (DbNumber d) ys = d : ys
        f _ ys          = ys

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb = (\(sum,cnt) -> (fromInteger sum) / (fromInteger cnt)) . foldr (\x (sum, cnt) -> (sum+x , cnt+1)) (0, 0) . filterDbNumber