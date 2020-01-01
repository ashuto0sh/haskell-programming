# Basic Datatypes

*Types* are a set of values along with a set of operations.



### Data Declaration

The *type constructor* is the name of the type and is capitalized.
*Data constructors* are the values that inhabit the type they are defined in.

```haskell
-- This is the Data Declaration for the type Bool
data Bool = False | True
-- [1]       [2] [3] [4]
```

1. Type constructor for datatype Bool. This is the name of the type and shows up in type signatures.
2. Data constructor for the value False.
3. Pipe | indicates a sum type or logical disjunction: or. So, a Bool value is True or False.
4. Data constructor for the value True.



### Numeric Types

#### Integral numbers

1. `Int`: Fixed precision integers.
2. `Integer`: Can represent arbitrarily large integers.
3. `Word`: Fixed-precision,  unsigned integers.

#### Fractional numbers

1. `Float`: Single-precision floating point numbers. Don't use them!
2. `Double`: The double-precision floating point number type. It has twice as many bits with which to describe numbers as the `Float` type.
3. `Rational`: Fractional number that represents a ratio of two integers. Arbitrarily precise.
4. `Fixed`
5. `Scientific`: This is a space efficient and almost arbitrary precision scientific number type.



```haskell
Prelude> import GHC.Int

Prelude> :t minBound 
minBound :: Bounded a => a
```



### Type class constraint

```haskell
Prelude> :t (/)
(/) :: Fractional a => a -> a -> a
```

The notation Fractional a => denotes a type class constraint. It tells us the type variable 𝑎 must implement the Fractional type class. Whatever type of number 𝑎 turns out to be, it must be a type that has an instance of the Fractional type class; that is, there must be a declaration of how the operations from that type class will work for the type. The / function will take one number that implements Fractional, divide it by another of the same type.

Values of `Fractional a => a` default to the floating point type `Double`.



### Comparing Values

```haskell
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
Prelude> :t (/=)
(/=) :: Eq a => a -> a -> Bool

Prelude> :t (<)
(<) :: Ord a => a -> a -> Bool
```



### Tuples

*Tuple* is a type that allows you to store and pass around multiple values within a single value.

```haskell
Prelude> :info (,)
data (,) a b = (,) a b  -- Defined in `GHC.Tuple'

fst :: (a, b) -> a
snd :: (a, b) -> b
```