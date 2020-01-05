# More Functional patterns

Functions are *applied* to arguments which *binds* their parameters to values. The fully applied function with its arguments is then *evaluated* to produce the output or result.



***Currying*** is the process of transforming a function that takes multiple arguments into a series of functions which each take one argument and return one result.



#### Binding variables to values

*Applying* a function binds its parameters to values. Type parameters are bound to a type and function variables are bound to values.

```haskell
addOne :: Integer -> Integer
addOne x = x + 1

addOne 1 -- x is now bound to 1
```

When `addOne` is applied to a value, we say that `x` is now bound to the value the function was applied to.



### Anonymous Functions

```haskell
triple :: Integer -> Integer
triple = (\x -> x * 3)-- :: Integer -> Integer
-- triple x = x * 3
```

You most often use this syntax when you’re passing a function in as an argument to a higher-order function.



### Pattern Matching

*Pattern matching* allows you to expose data and dispatch different behaviors based on that data in your function definitions by deconstructing values to expose their inner workings.

```haskell
isItTwo :: Integer -> Bool
isItTwo _ = False
isItTwo 2 = True -- Pattern match is redundant since catch all patter is exhaustive
```

The order of pattern matches matters! Try to order your patterns from most specific to least specific.

Incomplete pattern matches applied to data they don’t handle will return bottom, a non-value used to denote that the program cannot return a value or result. This will throw an exception, which if unhandled, will make your program fail.



### Case Expressions

```haskell
isPali :: String -> String
isPali xs = case (reverse xs) == xs of
    True -> "yes"
    False -> "no"
```



### Guard blocks

```haskell
myAbs :: Integer -> Integer
myAbs x
	| x < 0 = (-x)
	| otherwise = x
```



### Function composition

Function composition is a type of higher-order function that allows us to combine functions such that the result of applying one function gets passed to the next function as an argument.

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

and since -> associates to right

```haskell
(.) :: ((b -> c) -> ((a -> b) -> (a -> c)))
-- unpacking a couple ...
(.) :: (b -> c) -> (a -> b) -> (a -> c)
```

```haskell
*Office> :i $
($) :: (a -> b) -> a -> b       -- Defined in `GHC.Base'
infixr 0 $
```

Due to it's precedence, when used as infix, left and right of `$` is evaluated before this operator is applied.

