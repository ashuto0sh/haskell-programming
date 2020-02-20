import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
    foldr 
    (\x m ->
        case m of
            Nothing -> Just x
            Just y -> Just $ min x y
    ) Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
    foldr 
    (\x m ->
        case m of
            Nothing -> Just x
            Just y -> Just $ max x y
    ) Nothing

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

null'' :: (Foldable t) => t a -> Bool
null'' = getAll . foldMap (const (All False))

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ y -> y + 1) 0

length'' :: (Foldable t) => t a -> Int
length'' = getSum . foldMap (const (Sum 1))

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

toList'' :: (Foldable t) => t a -> [a]
toList'' = foldMap (: []) -- mappend [1] [2] == [1,2]

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr mappend mempty

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x y -> f x `mappend` y) mempty