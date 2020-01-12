# Building Projects

### Modules

Haskell programs are organized into modules. Modules contain the datatypes, type synonyms, type classes,  type class instances, and values you’ve defined at the top level. They offer a means to import other modules into the scope of your program, and they also contain values that can be exported to other  modules.

*Modules are analogous to namespaces in other languages.*



### Making packages with Stack

The *Haskell Cabal*, or Common Architecture for Building Applications and Libraries, *is a package manager*. A *package* is a program you’re building, including all of its modules and dependencies, whether you’ve written it or you’re building someone else’s program. A package has *dependencies* which are the interlinked elements of that program, the other packages and libraries it may depend on and any tests and documentation associated with the project. Cabal exists to help organize all this and make sure all dependencies are properly in scope.

*Stack* is a cross-platform program for developing Haskell projects. It is aimed at Haskellers both new and experienced, and it helps you manage both projects made up of multiple packages as well as individual packages, whereas Cabal exists primarily to describe a single package with a Cabal file that has the `.cabal` file extension.

*Stack is built atop Cabal but is relatively simpler*. Stack also relies on an LTS (long term support) snapshot of Haskell packages from Stackage3 that are guaranteed to work together, unlike packages



### Working with a basic project

```
F:\haskell\hpffp\hello>tree /F .
F:\HASKELL\HPFFP\HELLO
│   .gitignore
│   hello.cabal
│   LICENSE
│   README.md
│   Setup.hs
│   stack.yaml
│   stack.yaml.lock
│
└───src
        Main.hs
```

Above is the listing of a typical project directory structure. src: https://github.com/haskellbook/hello



```
> stack build
 -- build a project from root directory with .cabal file.
 
> stack ghci
 -- launch project aware GHCi REPL.
[... some other noise...]
Ok, modules loaded: Main.

Prelude> :l Main
 -- This GHCi is our project module aware.
[1 of 1] Compiling Main
Ok, modules loaded: Main.

Prelude> main
hello world
```



Stack knows what paths any executables might be located in, so using Stack’s exec command saves you the hassle of typing out a potentially verbose path.

```
$ stack exec -- hello
hello world
```



### A look at .cabal file

```
executable hello
-- 			[1]
	hs-source-dirs: 	src
	-- [2]
	main-is: 			Main.hs
	-- [3]
	default-language: 	Haskell2010
	-- [4]
	build-depends: 		base >= 4.7 && < 5
	-- [5]
```

1. This name following the declaration of an executable stanza tells Stack or Cabal what to name the binary or executable it creates.
2. Tells this stanza where to look for source code – in this case, the src subdirectory.
3. Execution of this binary should begin by looking for a main function inside a file named Main with the module name Main. ***Note that module names have to match filenames.*** Your compiler (not just Stack) will reject using a file that isn’t a Main module as the entry point to executing the program. Also note that it’ll look for the Main.hs file under all directories you specified in hs-source-dirs. Since we specified only one, it’ll find this in src/Main.hs, which is our only source file right now anyway.
4. Defines the version of the Haskell standard to expect. Not very interesting and doesn’t do much – mostly boilerplate, but necessary.
5. This is usually a meatier part of any Cabal stanza, whether it’s an executable, library, or test suite. This example (base) is really the bare minimum or baseline dependency in almost any Haskell project as you can’t really get anything done without the base library.



### Module Exports

```haskell
module Hello
    ( sayHello ) -- export list
    where

sayHello :: IO ()
sayHello = do
    putStrLn "Hello world"
```



```haskell
PS F:\haskell\hpffp\haskell-programming> stack ghci --ghci-options -XNoImplicitPrelude
> import Data.Bool (bool, not)
Data.Bool> :t not
not :: GHC.Types.Bool -> GHC.Types.Bool
Data.Bool> :t (&&)

<interactive>:1:1: error: Variable not in scope: &&
Data.Bool>
```

*You can selectively import only a subset of exported entities from a module.*

```haskell
Prelude> import qualified Data.Bool as B
Prelude> :t bool
	<interactive>:1:1:
	Not in scope: ‘bool’
	Perhaps you meant ‘Data.Bool.bool’
	
Prelude> :t Data.BooB> :i B.bool 
B.bool :: a -> a -> B.Bool -> a         -- Defined in `Data.Bool'

B> :i Data.Bool.bool  
B.bool :: a -> a -> B.Bool -> a         -- Defined in `Data.Bool'

B> :i Data.Bool.not  
B.not :: B.Bool -> B.Bool       -- Defined in `GHC.Classesl.not
```

*Qualified imports allow for disambiguating similar named entities coming from different modules.*