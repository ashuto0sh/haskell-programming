# Algebraic Datatypes



### Type and data constructors

**Type constructors** are used only at the type level, in *type signatures* and *type class declarations and instances*. Types are *static* and resolve at compile time. **Data constructors** construct the values at term level, values you can interact with at runtime. We call them constructors because they define a means of creating or building a type or a value.

```haskell
data Trivial = Trivial' 
--    [1]        [2] 
data UnaryTypeCon a = UnaryValueCon a 
--       [3]               [4]
```

1. Here the type constructor `Trivial` is like a constant value but at the type level. It takes no arguments and is thus *nullary*. The Haskell Report calls these type constants to distinguish them from type constructors that take arguments. 
2. The data constructor `Trivial'` is also like a constant value, but it exists in value, term, or runtime space. These are not three different things, but three different words for the same space that types serve to describe.
3. `UnaryTypeCon` is a type constructor of one argument. It’s a constructor awaiting a *type constant* to be applied to, but it has no behavior in the sense that we think of functions as having. Such type-level functions exist but are not covered in this book. 
4. `UnaryValueCon` is a data constructor of one argument awaiting a value to be applied to. Again, it doesn’t behave like a termlevel function in the sense of performing an operation on data. It’s more like a box to put values into. Be careful with the box/container analogy as it will betray you later – not all type arguments to constructors have value-level witnesses! Some are phantom.



We query the kind signature of a ***type constructor*** (*not a data constructor*) in GHCi with a :kind or :k.

```haskell
Prelude> :k Bool
Bool :: *
Prelude> :k [Int]
[Int] :: * 
Prelude> :k []
[] :: * -> *
```

```
type constructors -- compile-time 
-------------------- phase separation 
data constructors -- runtime
```



### What makes datatypes algebraic?

Algebraic datatypes in Haskell are algebraic because we can describe the patterns of argument structures using two basic operations: *sum* and *product*.

The **cardinality** of a datatype is the number of possible values it defines. *Nullary* constructors represent one value when reasoning about the cardinality of the types they inhabit. A unary data constructor takes one argument.

```haskell
data Example = MakeExample deriving Show
-- MakeExample is a nullary data constructor
data Goats = Goats Int deriving (Eq, Show)
-- Unary Data constructor
```



### newtype

*A newtype* defines a type that can only ever have a single unary data constructor. The cardinality of a `newtype` is the same as that of the type it contains. A `newtype` cannot be a product type, sum type, or contain nullary constructors.

Peculiarities:

- it has no runtime overhead, as it reuses the representation of the type it contains.
- one key contrast between a `newtype` and a *type alias* is that you can define type class instances for `newtype`s that differ from the instances for their underlying type.
- `newtype` can use the type class instances of the type `newtype` contains using a language extension called `GeneralizedNewtypeDeriving`.

```haskell
newtype Goats = Goats Int deriving (Eq, Show) 
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

Prelude> tooManyGoats (Cows 43)
-- error

class TooMany a where
	tooMany :: a -> Bool
instance TooMany Int where
	tooMany n = n > 42
instance TooMany Goats where
	tooMany (Goats n) = n > 43
-- differing type class instances for Goats and Int
```



### Sum Types

To know the cardinality of sum types, we add the cardinalities of their data constructors.

```haskell
length $ enumFrom (minBound :: Int8)
256
```



### Product Types

A ***product*** type’s cardinality is the product of the cardinalities of its inhabitants. Arithmetically, *products* are the result of multiplication. Where a sum type was expressing *or*, a product type expresses *and*.

#### Record Syntax

```haskell
data Person =
	Person { name :: String
		   , age :: Int }
	deriving (Eq, Show)

Prelude> :t name
name :: Person -> String
Prelude> papu = Person "Papu" 5
Prelude> name papu
"Papu"
```



### Normal Form

All the existing algebraic rules for products and sums apply in type systems.

```haskell
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show
data BookType = 
	FictionBook Fiction
	| NonfictionBook Nonfiction
	deriving Show
type AuthorName = String
data Author =
	Author (AuthorName, BookType)
-- Author is a product type, to make it normal form, 
-- we need to be sum of products, like below

type AuthorName = String
data Author = 
	Fiction AuthorName 
	| Nonfiction AuthorName 
	deriving (Eq, Show)
```



Construction and deconstruction of values form a duality. *Data is immutable in Haskell, so values carry with them the information about how they were created.* We can use that information when we consume or deconstruct the value.
