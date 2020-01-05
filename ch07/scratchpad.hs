addOneIfOdd :: Integer -> Integer
addOneIfOdd n = case odd n of
    True -> fd n
    False -> n
    where fd x = x + 1

addOneIfOdd2 :: Integer -> Integer
addOneIfOdd2 = \n -> case odd n of
    True -> fd n
    False -> n
    where fd n = n + 1 :: Integer

addFive :: Integer -> Integer -> Integer
addFive x y = (if x > y then y else x) + 5

addFive2 :: Integer -> Integer -> Integer
addFive2 = \x -> \y -> (if x > y then y else x) + 5

mFlip :: (a -> b -> c) -> b -> a -> c
mFlip fd = \x -> \y -> fd y x

mFlip2 :: (a -> b -> c) -> b -> a -> c
mFlip2 fd x y = fd y x

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

f1 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f1 (a, _, c) (d, _, f) = ((a, d), (c, f))

isPali :: String -> String
isPali xs = case (reverse xs) == xs of
    True -> "yes"
    False -> "no"