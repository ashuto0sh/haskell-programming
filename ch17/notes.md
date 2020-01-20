# Applicative

We’ve seen two common algebras that are used as type classes. *Monoid gives us a means of mashing two values of the same type together. Functor, on the other hand, is for function application over some structure we don’t want to have to think about*. Monoid’s core operation, `mappend`, smashes the structures together – when you `mappend` two lists, they become one list, so the structures themselves have been joined. However, the core operation of Functor, `fmap`, applies a function to a value that is within some structure while leaving that structure unaltered.

***Applicatives are Monoidal Functors.*** The Applicative type class allows for function application lifted over structure (like Functor). But with Applicative the function we’re applying is also embedded in some structure. Because the function and the value it’s being applied to both have structure, we have to smash those structures together. So, Applicative involves monoids and functors.



### Introduction

```haskell
class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
-- every type that can have an Applicative instance must also have a Functor instance.
```

The `pure` function lifts something into functorial (applicative) structure. You can think of this as being a bare minimum bit of structure or structural *identity*. `(<*>)` is an infix function called ‘apply’ or sometimes ‘ap’.

```haskell
liftA  :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

##### Functor vs. Applicative

```haskell
fmap :: (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b

-- law
fmap f x = pure f <*> x
```



### Applicative Laws

1. Identity:

   Here is the definition of the identity law: 

   ```haskell
   pure id <*> v = v
   ```

2. Composition:

   Here is the definition of the composition law for applicatives: 

   ```haskell
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
   ```

3. Homomorphism: 
   A *homomorphism* is a structure-preserving map between two algebraic structures. The effect of applying a function that is embedded in some structure to a value that is embedded in some structure should be the same as applying a function to a value without affecting any outside structure:

   ```haskell
   pure f <*> pure x = pure (f x)
   ```

4. Interchange:

   We begin again by looking at the definition of the interchange law: 

   ```haskell
   u <*> pure y = pure ($ y) <*> u
   ```

   