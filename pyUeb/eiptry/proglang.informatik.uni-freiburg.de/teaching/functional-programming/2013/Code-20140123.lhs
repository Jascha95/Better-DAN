> {-# LANGUAGE GADTs, KindSignatures #-}
> module Week1403_2 where

> import Week1403_1

> import Data.Char

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

----------------------------------------------------------------------

This representation of lambda expressions is called de Bruijn
representation. A lambda does not bind an explicit variable name. The
number in a variable refers to number of enclosing lambdas that need
to be skipped to find the binding lambda. That is, the variable 0
refers is bound by the next enclosing lambda. The variable 1 is bound
by the second enclosing lambda, and so on.

A program is a closed term, that is, a term without free variables.

> type Program a = FExp () a

> ex1 = Lam (Var Zero)   -- \lambda x.x        -- the I combinator
> ex2 = Lam (Lam (Var (Succ Zero))) -- \x\y.x  -- the K combinator
> ex3 = Lam (Lam (App (Var (Succ Zero)) (Var Zero))) -- \f\x. f x
> ex4 = Lam (Lam (Lam (App (App (Var (Succ (Succ Zero))) (Var Zero))
>                          (App (Var (Succ Zero)) (Var Zero)))))

the S combinator
S x y z = (x z) (y z)





> lkup :: Nat e a -> e -> a
> lkup Zero (a, b) = a
> lkup (Succ p) (_, b) = lkup p b

> lkup_pf :: Nat e a -> e -> a
> lkup_pf Zero = fst 
> lkup_pf (Succ p) = lkup_pf p . snd




> feval :: e -> FExp e a -> a
> feval e (App f x) = (feval e f) (feval e x)
> feval e (Lam b) = \x -> feval (x, e) b
> feval e (Var p) = lkup p e

> feval' :: FExp gamma a -> gamma -> a
> feval' (App f x) = \gamma -> ((feval' f) gamma) ((feval' x) gamma) -- S (feval'f) (feval' x) gamma
> feval' (Lam b)   = \gamma -> \x -> feval' b (x, gamma)
> feval' (Var p)   = lkup p 




This interpretation corresponds to the interpretation of the simply
typed lambda calculus in a CCC (cartesian closed category).


Types T are represented by objects [[T]] in the CCC.

The empty environment 0 is represented by the terminal object of the CCC.
[[0]] = ()

The extended environment x:T, Gamma is represented by the product
[[T]] x [[Gamma]].

A term of type 
Gamma |- e : T
is represented by an arrow [[Gamma]] -> [[T]]

A variable is mapped to a projection arrow. (cf. lkup_pf)
[[Zero]] = proj_1
[[Succ v]] = [[v]] . proj_2

Function application is mapped as follows:
[[App f x]] = eval . ([[f]] x [[x]]) . Delta id

[[f]] : [[Gamma]] -> [[S->T]] = [[Gamma]] -> [[T]]^[[S]]
[[x]] : [[Gamma]] -> [[S]]
Delta : A -> A x A (diagonal)
eval : B^A x A -> B

Lambda abstraction is mapped thus:
[[Lam b]] = iso . [[b]]

[[b]] :                             [[S]] x [[Gamma]] -> [[T]]
[[Lam b]] : [[Gamma]] -> [[S->T]] = [[Gamma]] -> [[T]]^[[S]]

These two types are connected by a natural isomorphism iso in a CCC!

S x G -> T = G x S -> T
G -> T^S   = G -> S -> T

At top level:
[[S]] x [[0]] -> [[T]]
= 
[[S]] x () -> [[T]]
=
[[S]] -> [[T]]

