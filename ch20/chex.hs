data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant a) = f a

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two _ b) = f b