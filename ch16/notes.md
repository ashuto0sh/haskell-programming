# Functors

```haskell
class Functor f  where 
[1]     [2]  [3]  [4]
	fmap :: (a -> b) -> f a -> f b 
	 [5]      [6]       [7]    [8]
```

1. class is the keyword to begin the definition of a type class.
2. `Functor` is the name of the type class we are defining.
3. Type classes in Haskell usually refer to a *type*. The letters themselves, as with type variables in type signatures, do not mean anything special. 𝑓 is a conventional letter to choose when referring to types that have functorial structure. The 𝑓 must be the same 𝑓 throughout the type class definition.
4. The `where` keyword ends the declaration of the type class name and associated types. After the where the operations provided by the type class are listed.
5.  We begin the declaration of an operation named `fmap`.
6. The argument a -> b is any Haskell function of that type (remembering that it could be an (a -> a) function for this purpose).
7. The argument f a is a `Functor` 𝑓 that takes a type argument 𝑎. That is, the 𝑓 is a type that has an instance of the `Functor` type class.
8. The return value is `f b`. It is the same 𝑓 from f a, while the type argument 𝑏 *possibly but not necessarily* refers to a different type.

```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```



### Functor Laws

###### Identity

The first law is the law of identity: 

`fmap id == id` 

*If we `fmap` the identity function, it should have the same result as passing our value to identity.*

```haskell
Prelude> fmap id "Hi Julie"
"Hi Julie"
Prelude> id "Hi Julie"
"Hi Julie"
```

###### Composition

The second law for Functor is the law of composition: 

`fmap (f . g) == fmap f . fmap g` 

This concerns the composability of `fmap`. If we compose two functions, 𝑓 and 𝑔, and `fmap` that over some structure, we should get the same result as if we `fmap`ped them and then composed them:

```haskell
Prelude> fmap ((+1) . (*2)) [1..5]
[3,5,7,9,11]
Prelude> fmap (+1) . fmap (*2) $ [1..5]
[3,5,7,9,11]
```

###### Structure preservation

Both of these laws touch on the essential rule that `functor` must be structure preserving.

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

The 𝑓 is constrained by the type class `Functor`, but that is all we know about its type from this definition. As we’ve seen with type class-constrained polymorphism, this still allows it to be any type that has an instance of `Functor`. We're basically lifting the function that over the `functor` type. To say, the function that requires a type argument now is lifted to work with `f a`.



### Terminology

*Higher-kinded polymorphism* is polymorphism which has a type variable abstracting over types of a higher kind. Functor is an example of higher-kinded polymorphism because the kind of the 𝑓 parameter to Functor is `* -> *`. Another example of higher-kinded polymorphism would be a datatype having a parameter to the type constructor which is of a higher kind, such as the following: 

```haskell
data Weird f a = Weird (f a)
```

Where the kinds of the types involved are: 

```haskell
a :: *
f :: * -> *
Weird :: (* -> *) -> * -> * 
```

Here both Weird and 𝑓 are higher-kinded, with Weird being an example of higher-kinded polymorphism.

###### Lifting

There are a couple of ways people commonly think about it. One is that we can *lift a function into a context*. Another is that we *lift a function over some layer of structure* to apply it. The effect is the same:

```haskell
Prelude> fmap (+1) $ Just 1
Just 2
Prelude> fmap (+1) [1, 2, 3]
[2,3,4]
```

In the first case, we lift that function into a Maybe context in order to apply it; in the second case, into a list context. It can be helpful to think of it in terms of lifting the function into the context, because it’s the context we’ve lifted the function into that determines how the function will get applied (to one value or, recursively, to many, for example). The context is the datatype, the definition of the datatype, and the Functor instance we have for that datatype. It’s also the contexts that determine what happens when we try to apply a function to an 𝑎 that isn’t there:

```haskell
Prelude> fmap (+1) []
[]
Prelude> fmap (+1) Nothing
Nothing
```

But we often speak more casually about lifting over, as in `fmap` lifts a function over a data constructor. This works, too, if you think of the data constructor as a layer of structure. The function hops over that layer and applies to what’s inside, if anything.

More precisely, lifting means applying a type constructor to a type, as in taking an `𝑎` type variable and applying an `𝑓` type constructor to it to get an `f a`.

