# Lists

The list datatype in Haskell is defined like this: 

```haskell
data [] a = [] | a : [a]
```

Here `[]` is the type constructor for lists as well as the data constructor for the empty list. The `[]` data constructor is a *nullary* constructor because it takes no arguments. The second data constructor, in contrast, has arguments. (`:`) is an infix operator usually called `cons` (short for construct). Here cons takes a value of type `a` and a list of type `[a]` and evaluates to `[a]`.



### Using ranges to construct lists

```haskell
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> enumFromTo 1 10
[1,2,3,4,5,6,7,8,9,10]
```



### List Comprehensions

***List comprehensions*** are a means of generating a new list from a list or lists. They come directly from the concept of set comprehensions in mathematics, including similar syntax. They must have at least one list, called the generator, that gives the input for the comprehension, that is, provides the set of items from which the new list will be constructed. They may have conditions to determine which elements are drawn from the list and/or functions applied to those elements.



```haskell
[  x^y   |   x <- [1..5], y <- [2, 3], x ^ y < 200]    
-- [1]  [2]        		[ 3 ]       	 [ 4 ]
```

1. This is the output function that will apply to the members of the list we indicate.
2. The pipe here designates the separation between the output function and the input. 
3. This is the input set: a generator list and a variable that represents the elements that will be drawn from that list.
4. Predicate to apply  to the generator list for filtering.



### Spines and non-strict evaluation

***Spine*** is the connective structure that ties the collection of values together. In the case of a list, the spine is usually textually represented by the recursive cons (:) operators. Given the data: [1, 2], we get a list that looks like: 

```haskell
1 : (2 : []) 

--   : <------|
--  / \       |
-- 1   : <----|This is the spine
--    / \     |
--   2  []    |
```

The `cons` (`(:)`) cells contain the individual values in a list. The way non-strict evaluation works, you can evaluate cons cells independently of what they contain. It is possible to evaluate only the spine of the list without evaluating individual values. It is also possible to evaluate only part of the spine of a list and not the rest of it.

Evaluation of the list in this representation proceeds *down* the spine. However, constructing the list (when that is necessary) proceeds *up* the spine. 

*Haskell’s evaluation is non-strict so a list isn’t constructed until it’s consumed. And nothing in the list is evaluated until it's forced!*

```haskell
Prelude> blah = enumFromTo 'a' 'z'
Prelude> :sprint blah
blah = _ 
Prelude> take 2 blah 
"ab"     
Prelude> :sprint blah
blah = 'a' : 'b' : _
```

Functions that are meta on lists generally do not force evaluation of individual elements and operate only on the spine of the list.

```haskell
Prelude> length [1,2,3,undefined,5,6]
6
Prelude> undefined 
*** Exception: Prelude.undefined
```



### Spines are evaluated independently of values

**Normal Form** : the expression is fully evaluated. 

**Weak Head Normal Form**’ : the expression is only evaluated as far as is necessary to reach a data constructor.

*Values in Haskell get reduced to weak head normal form by default.* 

```haskell
\x -> x * 10          -- WHNF & NF
(1,2) 		          -- WHNF & NF
"Papu" ++ "chon"      -- Neither
(1, "Papu" ++ "chon") -- WHNF but not NF
```

When we define a list and define all its values, it is in NF and all its values are known. We can also construct a list through ranges or functions. In this case, the list is in WHNF but not NF. The compiler only evaluates the head or first node of the graph, but just the cons constructor, not the value or rest of the list it contains.

```haskell
Prelude> x = [1,2] ++ undefined ++ [3,4]
Prelude> length x
*** Exception: Prelude.undefined
-- Note: sometimes, part of the spine itself could be undefined
```

```haskell
Prelude> take 2 $ filter odd [1, 2, 3, undefined] 
[1,3]
-- laazyyy
```



### Transforming list of values

```haskell
map :: (a -> b) -> [a] -> [b]
fmap :: Functor f => (a -> b) -> f a -> f b

map :: (a -> b) -> [a] -> [b]
map   _   []   = []
--   [1]  [2]    [3]
map  f  (x:xs) = f x   :  map f xs
--  [4]   [5]    [6]  [7]   [8]
```

1. `_` is used here to ignore the function argument because we don’t need it. 
2. We are pattern matching on the `[]` empty list case because List is a sum type with two cases and we must handle both every time we pattern match or case on a list value. 
3. We return the `[]` empty list value because when there are no values, it’s the only correct thing we can do. If you attempt to do anything else, the typechecker will swat you. 
4. We bind the function argument to the name 𝑓 as it merits no name more specific than this. 𝑓 and 𝑔 are common names for nonspecific function values in Haskell. This is the function we are mapping over the list value with map.
5. We do not leave the entire list argument bound as a single name. Since we’ve already pattern matched the `[]` empty list case, we know there must be at least one value in the list. Here we pattern match into the `(:)` second data constructor of the list, which is a product. 𝑥 is the single value of the cons product. 𝑥𝑠 is the rest of the list. 
6. We apply our function 𝑓 to the single value 𝑥. This part of the map function is what applies the function argument to the contents of the list. 
7. We `(:)` cons the value returned by the expression `f x` onto the head of the result of `map`’ing the rest of the list. Data is immutable in Haskell. When we map, we do not mutate the existing list, but build a new list with the values that result from applying the function. 
8. We call map itself applied to 𝑓 and 𝑥𝑠. This expression is the rest of the list with the function 𝑓 applied to each value.



### Filtering lists of values

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter pred (x:xs)
	| pred x = x : filter pred xs
	| otherwise = filter pred xs
	
Prelude> filter even [1..10]
[2,4,6,8,10]
```



### Zipping lists

```haskell
*Exercise> :t zip
zip :: [a] -> [b] -> [(a, b)]
*Exercise> :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```





In type theory, a *product type* is a type made of a set of types compounded over each other. In Haskell we represent products using tuples or data constructors with more than one argument. The “compounding” is from each type argument to the data constructor representing a value that coexists with all the other values simultaneously. Products of types represent a conjunction, “and,” of those types. If you have a product of `Bool` and `Int`, your terms will each contain a `Bool` and `Int` value. 

In type theory, a *sum type* of two types is a type whose terms are terms in either type, but not simultaneously. In Haskell sum types are represented using the pipe, `|`, in a datatype definition. Sums of types represent a disjunction, “or,” of those types. If you have a sum of `Bool` and `Int`, your terms will be *either* a `Bool` value or an `Int` value.