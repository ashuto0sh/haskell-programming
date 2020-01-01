# Strings

### A Look at Types

Types are a way of categorizing values. We can use `:type` in GHCi to learn about the type information of a value, expression or function. Datatypes in Haskell by default do not delimit the operations that can be performed on that data.

```haskell
Prelude> :type 'a'
'a' :: Char

Prelude> :type "Hello!"
"Hello!" :: [Char]
```

A `String` is a type alias for `[Char]`. `putStr` and `putStrLn` can be used to print strings to display.



### Functions on Lists

- `concat :: [[a]] -> [a]`: Takes a list of lists and concatenates them to form a single list.
- `head :: [a] -> a`: Returns first element from a list.
- `tail :: [a] -> [a]`: Returns list with head chopped off.
- `take :: Int -> [a] -> [a]`: Returns list of specified number of elements from start of the list.
- `drop :: Int -> [a] -> [a]`: Returns rest of list after dropping first n elements.
- `(!!) :: [a] -> Int -> a`: Return element at an index. 



### Scope

***Scope*** is where a variable referred to by name is valid.

***Local*** bindings are bindings local to particular expressions.

***Top level bindings*** in Haskell are bindings that stand outside of any other declaration.

