import Control.Monad

j :: Monad m => m (m a) -> m a
j x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f x = x >>= \y -> return $ f y

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a x f = f <*> x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (a:as) f = f a >>= \b -> (meh as f) >>= \bs -> return $ b : bs
