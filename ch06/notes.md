# Type Classes



[The Expression Problem](http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt)

*Type classes* and *types* in Haskell are, in a sense, opposites. Where a declaration of a type defines how that type in particular is created, a declaration of a type class defines how a set of types are consumed or used in computations.

```haskell
Prelude> :info Bool
data Bool = False | True
instance Bounded Bool
instance Enum Bool
instance Eq Bool
instance Ord Bool
instance Read Bool
instance Show Bool
```



*Note: Only types can be instantiated. Type classes are like interfaces with protected constructors. They can only be used in functions. The type class constraint is superfluous when the types are concrete.*

### Eq

```haskell
Prelude> :info Eq
class Eq a where
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
-- partial list
instance Eq a => Eq [a]
instance Eq Ordering
instance Eq Int
-- ...
```

**Type class deriving**: Type class instances we can magically derive are `Eq`, `Ord`, `Enum`, `Bounded`, `Read`, and `Show`, though there are some constraints on deriving some of these. Deriving means you don’t have to manually write instances of these type classes for each new datatype you create.

#### Writing type-class instances

```haskell
data Trivial a =
	Trivial' a

instance (Eq a) => Eq (Trivial a) where
	Trivial' a1 == Trivial' a2 = (a1 == a2)
```



### Num

```haskell
class Num a where  
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
        -- Defined in `GHC.Num'
instance Num Word -- Defined in `GHC.Num'
instance Num Integer -- Defined in `GHC.Num'
instance Num Int -- Defined in `GHC.Num'
--- ...
```

#### Integral

```haskell
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a) 
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}
        -- Defined in `GHC.Real'    
instance Integral Word -- Defined in `GHC.Real'   
instance Integral Integer -- Defined in `GHC.Real'
instance Integral Int -- Defined in `GHC.Real'
```

The type class constraint (`Real` a, `Enum` a) => means that any type that implements `Integral` must already have instances for `Real` and `Enum` type classes. Yes, to be `Real`, you must be `Num`.

#### Fractional

```haskell
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}
        -- Defined in `GHC.Real'
instance Fractional Float -- Defined in `GHC.Float' 
instance Fractional Double -- Defined in `GHC.Float'
```



### Printing and side effects

Haskell being *purely functional* means expressions in Haskell can be expressed exclusively in terms of a lambda calculus. A function like `putStr` is not just applied to the arguments that are in its scope but also asked to affect the world outside its scope in some way, namely by showing you its result on a screen. This is known as a *side effect*, a potentially observable result apart from the value the expression evaluates to.

Haskell makes effect bearing computations explicit (and themselves composable) and thus easier to reason about.

```haskell
ll> :t putStr   
putStr :: String -> IO ()
```

Stated as simply as possible, an I/O action (*IO*) is *an action* that, when performed, *has side effects*, including reading from input and printing to the screen and will contain *a return value*.

The `()` denotes an empty tuple, which we refer to as *unit*. *Unit* is a value, and also a type that has only this one inhabitant, that essentially represents nothing. To sum, `putStr` takes a `String` and returns an `IO` action, that, when run, prints the string on screen and returns a Unit(`()`).



#### Read

```haskell
ll> :t read
read :: Read a => String -> a
```

Don't use. Partial function. Throws. Bad.



![](.\type-class-heirarchy.PNG)