module Reverse where
import Data.List

rvrs :: String -> String
rvrs xs = intercalate " " $ reverse $ words xs

main :: IO()
main = print $ rvrs "Curry is awesome"