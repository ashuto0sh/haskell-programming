# Folding Lists

*Folds as a general concept are called catamorphisms.* Catamorphisms are a means of deconstructing data.



### Foldr

```haskell
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

```haskell
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldr (+) 0 [1, 2, 3]
-- gets expanded into
(+) 1 ((+) 2 ((+) 3 0))
```

```haskell
xs = map show [1..5]
y = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs

Prelude> y
"(1+(2+(3+(4+(5+0)))))"
-- Nice!! :P
```

One initially nonobvious aspect of folding is that it happens in two stages, *traversal* and *folding*. Traversal is the stage in which the fold recurses over the spine. Folding refers to the evaluation or reduction of the folding function applied to the values. *All folds recurse over the spine in the same direction*; the difference between left folds and right folds is in the association, or parenthesization, of the folding function and, thus, which direction the folding or reduction proceeds. `foldr`associated to the *right.*

```haskell
*FoldEx> foldr (\_ _ -> 9001) 0 ([1] ++undefined )
9001
*FoldEx> foldr (\_ _ -> 9001) 0 ([undefined] ++[1] )
9001
*FoldEx> foldr (\_ _ -> 9001) 0 (undefined ++[1] ) 
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\Err.hs:78:14 in base:GHC.Err
  undefined, called at <interactive>:25:25 in interactive:Ghci7
```

*Since foldr matches against (x:xs), it the first cons cell itself can't be undefined.  Notice the inhabitant of the cons cell can be undefined(bottom) but not the cons cell itself*



### Foldl

 

```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldl (+) 0 (1 : 2 : 3 : []) 
-- foldl associatesto left like this 
((0 + 1) + 2) + 3
```

```haskell
Prelude> f x y = concat ["(",x,"+",y,")"]

Prelude> foldl f "0" (map show [1..5]) 
"(((((0+1)+2)+3)+4)+5)"
```

​    

```haskell
foldr (^) 2 [1..3] 
(1 ^ (2 ^ (3 ^ 2))) 
(1 ^ (2 ^ 9)) 
1 ^ 512 
1

-- Contrast that with this: 

foldl (^) 2 [1..3] 
((2 ^ 1) ^ 2) ^ 3 
(2 ^ 2) ^ 3 
4 ^ 3 
64
```



### Comparison

An important difference between `foldr` and `foldl` is that a left fold has the successive steps of the fold as its first argument. The next recursion of the spine isn’t intermediated by the folding function as it is in `foldr`, which also means recursion of the spine is unconditional. Having a function that doesn’t force evaluation of either of its arguments won’t change anything.
