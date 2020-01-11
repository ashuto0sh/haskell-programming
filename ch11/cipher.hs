module Cipher where

import Data.Char

type KeyWord     = String
type PlainText   = String
type EncodedText = String

rotateChar :: Int -> Char -> Char
rotateChar n x
    | (x >= 'a' && x <= 'z') = chr ((k - a + n)  `mod` 26 + a)
    | (x >= 'A' && x <= 'Z') = chr ((k - aa + n) `mod` 26 + aa)
    | otherwise = x
    where
        k = ord x
        a = ord 'a'
        aa = ord 'A'

encode :: KeyWord -> PlainText -> EncodedText
encode _ [] = []
encode [] _ = error "Invalid parameter"
encode kss@(k:ks) (x:xs) 
    | isLetter x = rotateChar (f k - f 'a') x : encode ks xs
    | otherwise  = x : encode kss xs
    where
        f = ord . toUpper

vigenere :: KeyWord -> PlainText -> EncodedText
vigenere k xs = encode ks xs
    where ks = cycle $ map toUpper k