module Word where

import Data.Char

isVowel :: Char -> Bool
isVowel x
    | isUpper x         = isVowel $ toLower x
    | x `elem` "aeiou"  = True
    | otherwise         = False

countVowels :: String -> Integer
countVowels = foldr (\c n -> if isVowel c then n+1 else n) 0

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs
    | cv > cc = Nothing
    | otherwise = Just $ Word' xs
    where
        cv = countVowels xs
        cc = fromIntegral (length xs) - countVowels xs