module Person where

import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Exit (exitSuccess)

type Name = String
type Age  = Integer

data Person = Person Name Age --deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow 
    | otherwise =
        Left
        $ PersonInvalidUnknown
        $ "Name was: " ++ show name ++ " Age was: " ++ show age

instance Show Person where
    show (Person name age) =
        "Person { Name: " ++ name ++ ", Age: " ++ show age

instance Show PersonInvalid where
    show NameEmpty = "NameEmpty | Issue: You've given empty name sir"
    show AgeTooLow = "AgeTooLow | Issue: You've given weird age boi"
    show (PersonInvalidUnknown s) = "Unknown issue | Issue: You've are as much at fault as we are " ++ s

gimmePerson :: IO ()
gimmePerson = do
    putStr "Hi! \nMay I know your name pls boi? "
    name <- getLine
    putStr "And your age would be, luv? "
    ageStr <- getLine
    let age = read ageStr :: Integer
    case mkPerson name age of
        (Left err) -> do
            putStr $ "Incorrect inputs boi sir, try again later? " ++ show err
            exitSuccess
        (Right p) -> do
            putStrLn $ "Yay! Successfully got ya boi person: " ++ show p