module Palindrome where

import Control.Monad
import Data.Char
import Data.List
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)

normalizeString :: String -> String
normalizeString = map toLower . intercalate "" . words . filter isAlpha

palindrome :: IO ()
palindrome =
    forever $ do
        hSetBuffering stdout NoBuffering
        putStr "Enter the word to test: "
        line1 <- getLine
        case ((normalizeString line1) == (reverse $ normalizeString line1)) of
            True ->
                putStrLn "Yeah its a pali lol"
            False -> do
                putStrLn "Nope"
                exitSuccess