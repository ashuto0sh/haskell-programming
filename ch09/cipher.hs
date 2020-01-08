module Cipher where

import Data.Char

rotateChar :: Int -> Char -> Char
rotateChar a x
    | (x >= 'a' && x <= 'z') = chr (((ord x) - (ord 'a') + a) `mod` 26 + (ord 'a'))
    | (x >= 'A' && x <= 'Z') = chr (((ord x) - (ord 'A') + a) `mod` 26 + (ord 'A'))
    | otherwise = x

caesarCipher :: String -> String
caesarCipher = map $ rotateChar 3

unCaesarCipher :: String -> String
unCaesarCipher = map $ rotateChar 23