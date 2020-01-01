# Haskell Introduction

***GHCi*** is Haskell's REPL. Launched from command-line, it provides an interactive environment to enter Haskell code directly into.

***Prelude*** is Haskell's standard library. Loaded automatically into GHCi.



### GHCi commands

Non-Haskell commands specific to controlling and managing GHCi session.

***:quit,q*** : Quit session.

***:info,i*** : Info about an operator / function. The datatype definition for datatypes.

***:load,l*** : Load a Haskell source file into GHCi session.

***:reload,r*** : Reload the same source file.

***:module,m*** : Unload the current module.

***:type,t*** : Get the type of a value, expression, or function in GHCi.

***:set*** : Sets the prompt to specified string. Ex: `:set prompt "λ> "`



### Haskell Expressions

Everything in Haskell is an expression or a declaration. Expressions maybe values, combination of values or functions applied to values. Declarations allow us to name expressions.

We say that expressions are in *normal form* when there are no more evaluation steps that can be taken, i.e., when they’ve reached an irreducible form. Reducible expressions are also called *Redexes*.



### Parts of a function

```haskell
triple    x      =      x * 3 
--[1]    [2]    [3]     [ 4 ]
```



1. This is the name of the function we are defining; it is a function declaration. Note that it begins with a lowercase letter. 
2. This is the parameter of the function. The parameters of our function correspond to the head of a lambda and bind variables that appear in the body expression. 
3. The = is used to define (or declare) values and functions. This is not how we test for equality between two values in Haskell.
4. This is the body of the function, an expression that could be evaluated if the function is applied to a value.

Function names must start with a lower-case.

***Weak Head Normal Form*** implies that the expressions get evaluated to canonical/normal form only when the values are expected or looked at.



### Associativity and Precedence of Functions

```haskell
:info (*)
infixl  7   *
-- [1] [2] [3]
```

1. `infixl` means it’s an infix operator; the l means it’s left associative.
2. 7 is the precedence: higher is applied first, on a scale of 0-9.
3. Infix function name: in this case, multiplication.



### Notes on writing in source files

- Module names are Capitalized.
- Use spaces. No trailing whitespaces
- Expressions that re grouped together must be indented at the same level.  



`let` introduces an expression, so it can be used wherever you can have an expression, but `where` is a declaration and is bound to a surrounding syntactic construct.