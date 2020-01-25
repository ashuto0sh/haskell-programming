# Monad

```haskell
class Applicative m => Monad m where
	(>>=) :: m a -> (a -> m b) -> m b -- sequencing operator 
	(>>)  :: m a -> m b -> m b		  -- bind operator
	return :: a -> m a				  -- like pure
```

*A* `Monad` *has to be an instance of* `Applicative`. Given that `Monad` is stronger than `Applicative`, and `Applicative` is stronger than `Functor`, you can derive `Applicative` and `Functor` in terms of `Monad`, just as you can derive `Functor` in terms of `Applicative`. It means you can write `fmap` using monadic operations.

```haskell
xs >>= return . f == fmap f xs
Functor -> Applicative -> Monad
```

```haskell
fmap :: Functor f 	  => (a -> b) 	-> f a -> f b
<*>  :: Applicative f => f (a -> b) -> f a -> f b
>>=  :: Monad f 	  => f a -> (a -> f b) -> f b
```

```haskell
import Control.Monad (join)
join :: Monad m => m (m a) -> m a
-- compare
concat :: [[a]] -> [a]
```

##### Monad also lifts

```haskell
liftA :: Applicative f => (a -> b) -> f a -> f b
liftM :: Monad m => (a1 -> r) -> m a1 -> m r

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

-- similarly liftM3
```



### Do syntax and Monads

```haskell
Prelude> printOne = putStrLn "1"
Prelude> printTwo = putStrLn "2"
Prelude> twoActions = (printOne, printTwo)
Prelude> :t twoActions
twoActions :: (IO (), IO ())
Prelude> fst twoActions
1
```

Below demonstrates the equivalence of do notation and monadic binding/sequencing operations.

```haskell
bindingAndSequencing :: IO ()
bindingAndSequencing = do
	putStrLn "name pls:"
	name <- getLine
	putStrLn ("y helo thar: " ++ name)
	
bindingAndSequencing' :: IO ()
bindingAndSequencing' =
	putStrLn "name pls:" >>
	getLine >>=
	\name -> putStrLn ("y helo thar: " ++ name)
```

```haskell
doSomething =
	do
		a <- f
		b <- g
		c <- h
		pure (a, b, c) 
-- You can rewrite it using Applicative. 
-- On the other hand, if you have something like this: 
doSomething' n =
	do
		a <- f n
		b <- g a
		c <- h b
		pure (a, b, c)
-- You’re going to need Monad because 
-- 𝑔 and ℎ are producing monadic structure based on
-- values that can only be obtained by depending on 
-- values generated from monadic structure.
```

The long and short of it: 

	1. With the `Maybe Applicative`, each `Maybe` computation fails or succeeds independently of each other. You’re lifting functions that are also `Just` or `Nothing` over `Maybe` values.
 	2. With the `Maybe Monad`, computations contributing to the final result can choose to return `Nothing` based on previous computations.



### Examples of Monads

#### Maybe

```haskell
instance Monad Maybe where
	return x = Just x
	(Just x) >>= k = k x
	Nothing  >>= _ = Nothing
```

```haskell
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow n a w =
    noEmpty n >>=
        \vn -> noNegative a >>=
            \va -> noNegative w >>=
                \vw -> weightCheck $ Cow vn va vw
```

