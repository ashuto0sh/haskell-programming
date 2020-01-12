module LibEither where

lefts' :: [Either a b] -> [a]
lefts' =
    foldr
    (\x y ->
        case x of
            (Left x)  -> x:y
            (Right _) -> y
    )
    []

rights' :: [Either a b] -> [b]
rights' =
    foldr
    (\x y ->
        case x of
            (Right x) -> x:y
            (Left _)  -> y
    )
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' a2c _ (Left a)  = a2c a
either' _ b2c (Right b) = b2c b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' b2c = either' (\a -> Nothing) (\b -> Just $ b2c b)