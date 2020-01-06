module WordNumber where

import Data.List (intersperse)

digitToWord  :: Int -> String
digitToWord x = show x

digits :: Int -> [Int]
digits x
    | x <= 9    = [x]
    | otherwise = (digits $ fst dd) ++ [snd dd]
        where
            dd = divMod x 10

wordNumber :: Int -> String
wordNumber x = concat $ map digitToWord $ digits x