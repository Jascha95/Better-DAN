> {-# LANGUAGE ExistentialQuantification, GADTs, KindSignatures #-}
> module Week1403_2 where

> import Week1403_1

> import Data.Char

* Implementing arrows

Arrows may carry static information. That's an advantage above monads.
This static information can be exploited in an arrow for parsing,
where the static information might be the lookahead set of a parser,
which enables efficient implementation of alternatives.

Furthermore, an implementation of arrows may exploit static
information to optimize, for instance, by applying arrow laws on the
fly. As an example, consider the arrow law:

|  arr f >>> arr g = arr (f >>> g)

To implement that law, we might represent arrows constructed with arr
such that composition recognizes it and can take advantage.

> data Opt arrow a b = Arr (a -> b)
>                    | Lift (arrow a b)

Running such an optimized arrow means to interpret the constructors.

> runOpt (Arr f) = arr f
> runOpt (Lift f) = f

But now we can implement the Arrow operations in an optimizing way.

> instance Arrow0 arrow => Arrow0 (Opt arrow)  where
>   arr = Arr
>   Arr f >>> Arr g = Arr (f >>> g)
>   f >>> g = Lift (runOpt f >>> runOpt g)
> instance Arrow arrow => Arrow (Opt arrow) where
>   first (Arr f) = Arr (first f)
>   first (Lift f) = Lift (first f)

|  Arr f >>> (Arr g >>> Lift h)

But this optimizer has a significant shortcoming. Its optimization
depends on the way in which arrows are composed. In particular,
associativity matters. For example,

(Arr f >>> Arr g) >>> Lift h

simplifies to 

Arr (f >>> g) >>> Lift h

but 

Arr f >>> (Arr g >>> Lift h)

becomes

Arr f >>> Lift (arr g >>> h)

which misses the optimization.

This problem may be overcome by a more elaborate representation that
recognizes compositions consisting of two static arrows and a lifted
one:

> data Opt' arrow a b = Arr' (a -> b)
>                     | forall c d. ALA (a -> c) (arrow c d) (d -> b)

|  ALA f g h === arr f >>> g >>> arr h

This data definition uses an interesting twist ...

> instance Arrow0 arrow => Arrow0 (Opt' arrow) where
>   arr = Arr'
>   Arr' f >>> Arr' g = Arr' (f >>> g)
>   Arr' f >>> ALA gl h gr = ALA (f >>> gl) h gr
>   ALA fl h fr >>> Arr' g = ALA fl h (fr >>> g)
>   ALA fl h1 fr >>> ALA gl h2 gr = ALA fl (h1 >>> arr (fr >>> gl) >>> h2) gr

> instance Arrow arrow => Arrow (Opt' arrow) where
>   first (Arr' f) = Arr' (first f)
>   first (ALA fl h fr) = ALA (first fl) (first h) (first fr)

The good thing about this definition is that composition in the
underlying arrow is only used when composing ALA with ALA in the
expression

| (h1 >>> arr (fr >>> gl) >>> h2)

The bad thing is that it introduces a lot of fluff with each
nontrivial arrow, because lifting it into the Opt' arrow type requires
combining it with two identity functions. These functions give rise to
extra compositions that could be elided.

These extra compositions may be removed in at least two different
ways.
1. Three further constructors could be added to the Opt' type
representing the expressions
Lift f
Lift f >>> arr g
arr g >>> lift f
Then the composition operator for Opt' could be further specialized to
deal with these constructors.

2. Instead of using the function type directly, specific
representations could be introduced for select functions. For example:

> data Fun a b = Fun (a -> b) | Id | Const b

The datatype Fun would represent general function, identity
functions, and constant function is a recognizable way. Composition of
elements of the Fun datatype could take advantage of the laws for
composing identity functions and constant functions.

There is, however, a catch with this definition of the Fun
datatype. The type of the Id constructor is insufficiently
constrained, so that `Id :: Fun Bool Int' is accepted. Unfortunately,
this typing deficiency stops us from defining a typesafe composition
function and an interpreting function like runOpt for the Fun
type. Here is an attempt: 

| composeFun :: Fun a b -> Fun b c -> Fun a c
| composeFun Id (Fun h) = Fun h

This line implements that Id is a left identity, but it is not
accepted! To see why, consider the following typings 

Id :: Fun Bool Int
h  :: Int -> Int
Fun h :: Fun Int Int
composeFun Id (Fun h) :: Fun Bool Int

Clearly, given just h :: Int -> Int it is impossible to produce a
function of type Bool -> Int.

The solution is to give the Id constructor a more restricted
typing. This gives rise to generalized algebraic datatypes
(GADTs). See next lecture. 

  Closing remark:

  Opt is an example of an arrow transformer analogous to a monad
  transformer. A type class may be defined capturing this concept...

* Generalized Algebraic Datatypes (GADTs)

Here is a version of the Fun datatype that enforces the restriction
that Id has type Fun a a. Its syntax generalizes the data definition
by allowing the specification of a full type signature for each
constructor. The only restriction on this signature is that the return
type of the constructor must be an instance of the declared data type.
This syntax is only enabled with the language extension GADTS (see
first line of file).

> data Fun' a b where
>   Fun' :: (a -> b) -> Fun' a b
>   Id' :: Fun' a a
>   Const' :: b -> Fun' a b

Pattern matching against a constructor of a GADT reifies the
constraint on the return type. In particular, pattern matching on the
Id' constructor introduces the constraint a ~ b for the right side of
the Id' equation. Hence, its type must be (a -> a). The remaining
constructors do not require the GADT extension.

> runFun' :: Fun' a b -> (a -> b)
> runFun' (Fun' f) = f
> runFun' Id' = id
> runFun' (Const' b) = const b

Using the GADT also enables writing the typesafe (symbolic)
composition function. 

> composeFun :: Fun' a b -> Fun' b c -> Fun' a c
> composeFun (Fun' f) (Fun' g) = Fun' (g . f)
> composeFun Id' h = h
> composeFun f Id' = f
> composeFun (Const' b) (Fun' f) = Const' (f b)
> composeFun f (Const' c) = Const' c

Original motivation for GADTs:
Efficient implementation of typed interpreters
Consider an interpreter with two base types, Int and Bool.
The traditional approach requires a value representation for the
return type of the interpreter. 
First, we consider a very simple language with just integers and booleans.

> data Value = Int Int | Bool Bool
>   deriving Show

> data Exp0 = Num0 Int | Add0 Exp0 Exp0 | Truth0 Bool | If0 Exp0 Exp0 Exp0

> eval0 :: Exp0 -> Value
> eval0 (Num0 i) = Int i
> eval0 (Add0 e1 e2) = case eval0 e1 of 
>                      Int i1 -> case eval0 e2 of
>                                  Int i2 -> Int (i1+i2)
> eval0 (Truth0 t) = Bool t
> eval0 (If0 e1 e2 e3) = case eval0 e1 of 
>                        Bool b1 -> if b1 then (eval0 e2) else (eval0 e3)

This interpreter crashes when we try to run type incorrect programs
like the following examples.

| eval0 (Add0 (Num0 42) (Truth0 False))
| eval0 (If0 (Num0 21) (Num 0) (Num 1))

With a GADT we can internalize the notion of a typed
expression. This way, a type-incorrect expression is already rejected
by Haskell.

> data Exp a where
>   Num :: Int -> Exp Int
>   Add :: Exp Int -> Exp Int -> Exp Int
>   Truth :: Bool -> Exp Bool
>   If :: Exp Bool -> Exp a -> Exp a -> Exp a

The above examples for type incorrect programs are rejected outright.

Furthermore, a so-called "tag-free" version of the interpreter can be
written that elides the Value datatype. 

> eval :: Exp a -> a
> eval (Num i) = i
> eval (Add e1 e2) = eval e1 + eval e2
> eval (Truth b) = b
> eval (If e1 e2 e3) = if eval e1 then eval e2 else eval e3

So far, we have punted around the function datatype because
this approach is not easily generalizable to functions:

|  data FExp a where
|   Var :: FExp a
|   Lam :: FExp b -> FExp (a -> b)
|   App :: FExp (a -> b) -> FExp a -> FExp b

This definition does not work because it does not provide sufficient
(type) information for the Lam and Var constructors. Where does the
type a in the Var constructor come from? What matches its type to,
say, a surrounding lambda abstraction? How does a Lam impose the type
a on the abstracted variable?

Intermezzo: the App constructor uses a feature called "existential
dataypes". Its presence can be detected by observing that the type
variable a occurs in the arguments of the constructor, but not in its
result type. Such a type variable is "existentially bound" and it can
be used to implement data abstraction: 

> data Package where
>   Package :: (a -> Int) -> a -> Package

The type variable is existentially quantified because it does not
occur in the return type of the constructor.

We may construct packages that instantiate a differently, but they all
have the same type Package (and may be put together in a list, for instance).

> package1 = Package (+1) 41
> package2 = Package length [1,2,3]
> package3 = Package ord 'C'

Unpackaging via pattern matching guarantees that f is a function
applicable to a and returning an Int, but nothing more is known about
the type of a. This type is not compatible (equal) to any other type.

> unpack (Package f a) = f a

Back to GADTs:

To generalize to functions, we need to use a more refined type for
expressions. It has to keep track of the result type and of the types
of its free variables.

> data FExp :: * -> * -> * where
>   App :: FExp e (a -> b) -> FExp e a -> FExp e b
>   Lam :: FExp (a, e) b -> FExp e (a -> b)
>   Var :: Nat e a -> FExp e a

> data Nat e a where
>   Zero :: Nat (a, b) a
>   Succ :: Nat e a -> Nat (b, e) a
