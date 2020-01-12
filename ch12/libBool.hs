module LibBool where

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just x) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ a2b (Just a) = a2b a
mayybee b _ Nothing    = b

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a Nothing  = a

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | length xs == length (catMaybes xs) = Nothing
    | otherwise                          = Just $ catMaybes xs