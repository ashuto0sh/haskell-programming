module Exercises where

import Data.Char
import Data.List

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf kss@(k:ks) xss
    | k `elem` xss = isSubseqOf ks xss
    | otherwise   = isSubseqOf kss xss

capitalizeWords :: String -> [(String, String)]
capitalizeWords = foldr (\xss@(x:xs) ys -> (xss, toUpper x : xs) : ys) [] . words

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

splitOn :: Char -> String ->  [String]
splitOn c xs = 
    foldr 
        (\x yss@(y:ys) ->
            if x == c
                then "" : yss
            else
                (x:y):ys
        )
        [""]
        xs

sanitize :: String -> String
sanitize "" = ""
sanitize xss@(x:xs)
    | isSpace x = sanitize xs
    | otherwise = xss

capitalizeParagraph :: String -> String
capitalizeParagraph = intercalate ". " . map capitalizeWord . map sanitize . splitOn '.'