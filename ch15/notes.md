# Monoid, Semigroup

###### *Simplicity does not precede complexity, but follows it.*

Algebra studies one or more operations and the set they operate over. In Haskell, these algebras can be implemented with type classes?; the type classes define the set of operations. Monoid is one of the algebras.



### Monoid

###### *A monoid is a binary associative operation with an identity.*

In plain English, a monoid is a function that takes two arguments and follows two laws: associativity and identity. Associativity means the arguments can be regrouped (or re-parenthesized, or re-associated) in different orders and give the same result, as in addition. Identity means there exists some value such that when we pass it as input to our function, the operation is rendered moot and the other value is returned, such as when we add zero or multiply by one. Monoid is the type class that generalizes these laws across types.

Type classes give us a way to recognize, organize, and use common functionalities and patterns across types that differ in some ways but also have things in common. The `Monoid` type class recognizes and orders a different pattern than `Num` but the goal is similar. The pattern of `Monoid` is outlined above: types that have *binary functions* that let you join things together in accordance with the laws of *associativity*, along with an *identity* value that will return the other argument unmodified. This is the pattern of summation, multiplication, and list concatenation, among other things. The type class abstracts and generalizes the pattern so that you write code in terms of any type that can be monoidally combined.

```haskell
class Semigroup m => Monoid m where
	mempty :: m
	mappend :: m -> m -> m
	mconcat :: [m] -> m
	mconcat = foldr mappend mempty
	
instance Semigroup [a] where
	(<>) = (++)
```

##### Why Integer doesn’t have a Monoid

While in mathematics the monoid of numbers is summation, there’s not a clear reason why it can’t be multiplication. Both operations are monoidal (binary, associative, having an identity value), but each type should only have one unique instance for a given type class, not two (one instance for a sum, one for a product).

To resolve the conflict, we have the `Sum` and `Product` newtypes to wrap numeric values and signal which Monoid instance we want. These newtypes are built into the `Data.Monoid` module. While there are two possible instances of `Monoid` for numeric values, we avoid using scoping tricks and abide by the rule that type class instances are unique to the types they are for:

```haskell
Prelude> mappend (Sum 1) (Sum 5) 
Sum {getSum = 6} 
Prelude> mappend (Product 5) (Product 5) 
Product {getProduct = 25} 
Prelude> mappend (Sum 4.5) (Sum 3.4) 
Sum {getSum = 7.9}
```

*Integers form a monoid under summation and multiplication.*

##### Why newtype

1. To signal intent: using `newtype` makes it clear that you only intend for it to be a wrapper for the underlying type. The `newtype` cannot eventually grow into a more complicated sum or product type, while a normal datatype can. 
2. To improve type safety: avoid mixing up many values of the same representation, such as Text or Integer.
3. To add different type class instances to a type that is otherwise unchanged representationally, such as with Sum and Product.



### Laws

A `Monoid` must abide by the following laws

```haskell
-- left identity
mappend mempty x = x 

-- right identity 
mappend x mempty = x 

-- associativity 
mappend x (mappend y z) = mappend (mappend x y) z 

mconcat = foldr mappend mempty
```

#### The problem of orphan instances

An orphan instance is when an instance is defined for a datatype and type class, but not in the same module as either the declaration of the type class or the datatype. *If you don’t own the type class or the datatype, newtype it!*

```haskell
Duplicate instance declarations: 
instance Monoid (Listy a) 
-- Defined at Listy.hs:7:10 
instance Monoid (Listy a) 
-- Defined at ListyInstances.hs:5:10
```

There are a few solutions for addressing orphan instances:

1. You defined the type but not the type class? Put the instance in the same module as the type so that the type cannot be imported without its instances. 
2. You defined the type class but not the type? Put the instance in the same module as the type class definition so that the type class cannot be imported without its instances.
3. Neither the type nor the type class are yours? Define your own `newtype` wrapping the original type and now you’ve got a type that “belongs” to you for which you can rightly define type class instances. There are means of making this less annoying which we’ll discuss later.

*A type must have a unique (singular) implementation of a type class in scope, and avoiding orphan instances is how we prevent conflicting instances.*