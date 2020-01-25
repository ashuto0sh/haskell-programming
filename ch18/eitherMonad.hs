data Sum a b =
    First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First x) = First x
    fmap f (Second x) = Second $ f x
    
instance Applicative (Sum a) where
    pure x = Second x

    (<*>) _ (First x)            = First x
    (<*>) (First ff) _           = First ff
    (<*>) (Second fs) (Second x) = Second $ fs x
    
instance Monad (Sum a) where
    return = pure

    (Second b) >>= f = f b
    (First a)  >>= _ = First a
