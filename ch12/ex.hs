module Exercises where

import Data.Char
import Data.List

notThe :: String -> Maybe String
notThe xs
    | [toLower x | x <- xs] == "the" = Nothing
    | otherwise           = Just xs

replaceThe :: String -> String
replaceThe = intercalate " " . foldr
    (\x y -> 
        case x of
            Just l  -> l:y
            Nothing -> "a":y
    ) [] . map notThe . words

replaceThe2 :: String -> String
replaceThe2 = intercalate "" . foldr
    (\x y -> 
        case x of
            Just l  -> l:y
            Nothing -> "$":y
    ) [] . map notThe . words

isVowel :: Char -> Bool
isVowel x
    | isUpper x         = isVowel $ toLower x
    | x `elem` "aeiou"  = True
    | otherwise         = False

countTheBeforeVowelHelper :: String -> Integer
countTheBeforeVowelHelper []         = 0
countTheBeforeVowelHelper (_:[])     = 0
countTheBeforeVowelHelper ('$':k:ks) = if isVowel k then 1 + countTheBeforeVowelHelper ks else countTheBeforeVowelHelper (k:ks)
countTheBeforeVowelHelper (k:ks)     = countTheBeforeVowelHelper ks

countTheBeforeVowel = countTheBeforeVowelHelper . replaceThe2

countVowels :: String -> Integer
countVowels = foldr (\c n -> if isVowel c then n+1 else n) 0

myIterate :: (a -> a) -> a -> [a]
myIterate f a = [a] ++ myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = 
    case f b of
        Nothing -> []
        Just (a, b) -> [a] ++ myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, f x)) x