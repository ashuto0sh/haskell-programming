# Introduction to lambda calculus



***Expressions*** are combinations of concrete values, variable or functions.

***Functions*** are expressions that are applied to an argument and can be thus reduced or evaluated.

Haskell is a ***pure*** functional language, because it's has exact equivalence to lambda calculus.

***Referential Transparency*** means that a function,  given same input, will always return the same result.



### Lambda Expressions

***Lambda Expression*** can be a variable name, an abstraction (function), or a combination of those things.

***Abstraction*** consists of a head and a body. The variable named in head is the parameter and binds all instances of the variable in the body.

		𝜆𝑥.𝑥

***Application*** is the act of applying a lambda expression to an argument.



### Alpha Equivalence

𝜆𝑥.𝑥 == 𝜆𝑑.𝑑



### Beta Reduction

***Beta Reduction*** is the process of applying a lambda expression / function to an argument, thus substituting the input expression (argument) for all instances of the bound variables within the body of expression.

		(𝜆𝑥.𝑥)(𝜆𝑦.𝑦)
		[𝑥 ∶= (𝜆𝑦.𝑦)]
		𝜆𝑦.𝑦

Applications in lambda calculus are *left-associative*  ((𝜆𝑥.𝑥)(𝜆𝑦.𝑦)𝑧 <==> ((𝜆𝑥.𝑥)(𝜆𝑦.𝑦))𝑧)



### Free Variables

***Free Variables*** are the variables in the body of a function / abstraction / expression that is not bound to any argument in the head of expression. For example, in the expression (𝜆𝑥.𝑥𝑦)𝑧, 𝑧 is a free variable and 𝑥 is a bound variable.



A lambda expression can only bind one parameter and thus can accept only one argument. To mean, 𝜆𝑥𝑦.𝑥𝑦 <==> 𝜆𝑥𝑦.𝑥𝑦.



- An expression is said to be in ***beta normal form*** if it cannot be beta reduced any further. That is, the expression could not be applied to an argument anymore.
- ***Combinator*** is a lambda term with no free variables.
- ***Divergent Expression*** is a fully applied lambda expression that cannot be reduced to it's normal form.
- ***Normal order*** is a common evaluation strategy in lambda calculi. Normal order means evaluating the leftmost, outermost lambdas first, evaluating terms nested within after you’ve run out of arguments to apply.