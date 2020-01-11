module Phone where

import Data.Char
import Data.List

type Button = String

type Digit = Char
type Presses = Int

data DaPhone = DaPhone [Button]

phoneLayout :: [Button]
phoneLayout = ["1", "ABC2", "DEF3", "GHI4", "JKL5", "MNO6", "PQRS7", "TUV8", "WXYZ9", "*", " 0", ".,#"]

getNumPressesForChar :: Button -> Char -> [Char]
getNumPressesForChar b c = take k (repeat $ last b)
    where
        k = case c `elemIndex` b of
                Just x -> x + 1
                Nothing ->  error "asdasd"

getButtonForChar :: Char -> [Char]
getButtonForChar c = getNumPressesForChar (foldr (\x y -> if y == "" && c `elem` x then x else y) "" phoneLayout) c

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps _ c
    | isUpper c = [('*', 1), (head xs, length xs)]
    | otherwise = [(head xs, length xs)]
    where xs = getButtonForChar $ toUpper c