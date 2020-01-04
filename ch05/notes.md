# Types



Haskell is *statically typed*, i.e., type-checking happens ay compile time. 



### Function Type

*Functions* are values.

The arrow (->) is an *infix* operator that associates to the right. i.e., 
`a->b->c` == `a->(b->c)`.

```haskell
Prelude> fifteen = 15
Prelude> :t fifteen
fifteen :: Num a => a
Prelude> fifteenInt = fifteen :: Int
Prelude> fifteenDouble = fifteen :: Double
Prelude> :t fifteenInt
fifteenInt :: Int
Prelude> :t fifteenDouble
fifteenInt :: Double
```

***Question based on above demo***:
*Num* is the type-class that is like the base class in OO languages. What I mean is when I write a polymorphic function, I take a *type variable* that is constrained by *Num* type class. However, as seen above, casting `fifteen` as Int or Double works. Isn't it equivalent to down-casting in OO languages? wouldn't some information (a bunch of Double type specific  functions in this case) be lost when I do that? [StackOverflow Question](https://stackoverflow.com/questions/59580509/the-type-specification-operator-is-like-down-casting-in-object-oriented-language)



### Currying

All functions in Haskell take one argument and return one result. But since the arrow operator (->) associates to the right, we get a nice convenience.

`a->b->c->d` == `a->(b->(c->d))`. To read, the outer `->` takes an argument of type `a` and returns an other function of type `b->(c->d)`...

- ***Uncurried functions***: One function, many arguments `(a,b)->c`
- ***Curried functions***: Many functions, one argument apiece `a->(b->c)`

```haskell
Prelude> :t curry   
curry :: ((a, b) -> c) -> a -> b -> c
Prelude> :t uncurry 
uncurry :: (a -> b -> c) -> (a, b) -> c
Prelude>
```

***Sectioning*** refers to partial application of infix operators, which has a special syntax and allows you to choose whether the argument you’re partially applying the operator to is the first or second argument.

```haskell
Prelude> x = 5
Prelude> y = (2^)
Prelude> z = (^2)
Prelude> y x 32
Prelude> z x 25
```

```haskell
Prelude> f :: (Ord a, Num b) =>  a->b->a; f = undefined
Prelude> :t f 1 2  
f 1 2 :: (Ord a, Num a) => a
```



### Polymorphism

*Polymorphic type variables* give us the ability to implement expressions that can accept arguments and return results of different types without having to write variations on the same expression for each type.

In Haskell, polymorphism divides into two categories: *parametric polymorphism* (not constrained by any type-class) and *constrained polymorphism* (ad-hoc polymorphism).

*Ad-hoc polymorphism* is polymorphism that applies one or more type class constraints to what would’ve otherwise been a parametrically polymorphic type variable.

If a variable represents a set of possible values, then a type variable represents a set of possible types. When there is no type class constraint, the set of possible types a variable could represent is effectively unlimited. Type class constraints limit the set of potential types (and, thus, potential values) while also passing along the common functions that can be used with those values.

Cue: [Theorems for free](https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf)