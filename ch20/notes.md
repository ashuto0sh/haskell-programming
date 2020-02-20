# Foldable

*A list fold is a way to reduce the values inside a list to one summary value by recursively applying some function.*

### Foldable Class

`Foldable` typeclass is a class of data structures that can be folded to a summary value.

```haskell
class Foldable t where
	{-# MINIMAL foldMap | foldr #-}
```

The `MINIMAL` annotation on the type class tells you that a minimally complete definition of the type class will define `foldMap` or `foldr` for a datatype.

The `Foldable` type requires a higher-kinded type `* -> *`. 

```haskell
class Foldable (t :: * -> *) where
	fold :: Monoid m => t m -> m
	foldMap :: Monoid m => (a -> m) -> t a -> m
```

