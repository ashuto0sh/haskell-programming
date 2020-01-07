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

