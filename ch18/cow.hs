module Cow where

import Control.Monad

data Cow =
    Cow 
    {
        name :: String
      , age :: Int
      , weight :: Int 
    } deriving (Eq, Show)
    
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let
        w = weight c
        n = name c
    in
        if n == "Bess" && w > 499
            then Nothing
            else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w =
    noEmpty n >>=
        \vn -> noNegative a >>=
            \va -> noNegative w >>=
                \vw -> weightCheck $ Cow vn va vw

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' n a w =
    do
        vn <- noEmpty n
        va <- noNegative a
        vw <- noNegative w
        weightCheck $ Cow vn va vw

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' n a w =
    case noEmpty n of
        Nothing -> Nothing
        Just vn ->
            case noNegative a of
                Nothing -> Nothing
                Just va ->
                    case noNegative w of
                        Nothing -> Nothing
                        Just vw -> weightCheck $ Cow vn va vw

