> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
> module Code20140206 where

* Generic programming in Haskell

Based on

Ralf Hinze
Generics for the Masses
ICFP 2004

Oliveira, Hinze, Löh
Extensible and Modular Generics for the Masses
TFP 2007

See also http://www.cs.uu.nl/wiki/GenericProgramming/EMGM

What is generic (polytypic) programming?

The quest to define functions that work uniformly on all datatypes
*including those that are yet to be defined in the future*.
Typical examples are pretty printers, parsers, size functions,
equality, and so on.
In fact, Haskell's deriving mechanism is a piece of generic
programming, but it is hardwired in the Haskell compiler. Here, we are
interested in user-definable generic programs.

** Example: Data Compression

The idea is to map arbitrary values to sequences of bits.

> type Bin = [Bit]
> data Bit = O | I deriving Show
> bits :: Enum a => Int -> a -> Bin

The expression 'bits n x' represents an element x of an enumerated
type by a bit string of length n. The goal of this exercise is to lift
the bits function from just enums to a function showBin that works on
all types (exclusing function types).

To do so it is sufficient to define showBin for all primitive types
and for the elementary type constructors: the unit type, the sum type,
and the product type.

> data Unit = Unit
> data Plus a b = Inl a | Inr b
> data Pair a b = Pair { outl :: a, outr :: b }

To get from the actual type to these constructors requires to define
an isomorphism that maps a type into its elementary representation and
back.

> data Iso a b = Iso { fromData :: b -> a, toData :: a -> b }

The type signature of the generic function is (unusually) specified by
a data definition. 

> newtype ShowBin a = ShowBin { showBin' :: a -> Bin }

It must not be a "real" polymorphic function because it does not work
(uniformly) for all types. A generic function has so-called
*intensional polymorphism* where the function analyzes the type of its
argument to determine the action.  Hence, the argument type of a
generic function is restricted (using a type class) to those types
that have a representation that the generic mechnism can deal
with. This representation for type a is provided as a value of type
ShowBin a.

So every generic function is defined as an instance of a type class
Generic where the members return a suitable representation.

> instance Generic ShowBin where
>   unit     = ShowBin (\_ -> [])
>   plus a b = ShowBin (\x -> case x of Inl l -> O : showBin' a l
>                                       Inr r -> I : showBin' b r)
>   pair a b = ShowBin (\x -> showBin' a (outl x) ++ showBin' b (outr x))
>   char     = ShowBin (\c -> bits 7 c)
>   int      = ShowBin (\i -> bits 16 i)
>   view iso = \r -> ShowBin (\x -> showBin' r (fromData iso x))

** Tasks
*** generic comparison :: a -> a -> Ordering
*** readBin :: Bin -> a, which is an inverse to showBin

** Representing a type

Consider a typical datatype

> data Tree a = Leaf a | Fork (Tree a) (Tree a)

> type TreeF a = Plus a (Pair (Tree a) (Tree a)) 

need to construct isomorphism from
Tree a <-> TreeF a

> isoTree = Iso fromTree toTree

> fromTree :: Tree a -> TreeF a
> fromTree (Leaf a) = Inl a
> fromTree (Fork l r) = Inr (Pair l r)

> toTree :: TreeF a -> Tree a
> toTree (Inl a) = Leaf a
> toTree (Inr (Pair l r)) = Fork l r

For use in a generic function an appropriate representation function
is needed.

> rTree :: Generic f => f a -> f (Tree a)
> rTree a = view isoTree (plus a (pair (rTree a) (rTree a)))

Encoding the standard list datatype.


| data List a = Nil | Cons a (List a)

> type ListF a = Plus Unit (Pair a [a])

| iso :: [a] <-> ListF a

> isoList = Iso fromList toList

> fromList :: [a] -> ListF a 		-- Plus Unit (Pair a [a])
> fromList [] = Inl Unit
> fromList (x:xs) = Inr (Pair x xs)

> toList :: ListF a -> [a]
> toList (Inl Unit) = []
> toList (Inr (Pair x xs)) = x : xs

| rList' a = plus unit (pair a (rList' a))

Here is the corresponding representation transformer.

> rList :: Generic f => f a -> f [a]
> rList a = view isoList (plus unit (pair a (rList a)))

> type MaybeF a = Plus Unit a

| iso :: Maybe a <-> MaybeF a

> isoMaybe = Iso fromMaybe toMaybe

> fromMaybe Nothing = Inl Unit
> fromMaybe (Just a) = Inr a

> toMaybe (Inl Unit) = Nothing
> toMaybe (Inr a) = Just a

> rMaybe :: Generic f => f a -> f (Maybe a)
> rMaybe a = view isoMaybe (plus unit a)


Booleans. Bool = Plus Unit Unit

> isoBool = Iso fromBool toBool

> fromBool :: Bool -> Plus Unit Unit
> fromBool False = Inl Unit
> fromBool True  = Inr Unit

> toBool :: Plus Unit Unit -> Bool
> toBool (Inl Unit) = False
> toBool (Inr Unit) = True

> rBool :: Generic f => f Bool
> rBool = view isoBool (plus unit unit)


** Task: provide REP instances for
*** data Shrub α β = Tip α | Node (Shrub α β) β (Shrub α β)
*** data Rose α = Branch α [Rose α] 

** Implementation

> class Generic f where
>   unit   :: f Unit
>   plus   :: f a -> f b -> f (Plus a b)
>   pair   :: f a -> f b -> f (Pair a b)
>   constr :: Name -> Arity -> f a -> f a
>   constr _ _ = id
>   char   :: f Char
>   int    :: f Int
>   view   :: Iso a b -> f a -> f b


** Automatically inferring a representation

> class Rep a where
>   rep :: (Generic f) => f a

> instance Rep Unit where
>   rep = unit

> instance (Rep a, Rep b) => Rep (Plus a b) where
>   rep = plus rep rep

> instance (Rep a, Rep b) => Rep (Pair a b) where
>   rep = pair rep rep

> instance Rep Char where
>   rep = char

> instance Rep Int where
>   rep = int

> instance Rep a => Rep [a] where
>   rep = rList rep

> instance Rep a => Rep (Tree a) where
>   rep = rTree rep

> instance Rep Bool where
>   rep = rBool


> showBin :: (Rep a) => a -> Bin
> showBin = showBin' rep

** Problem: Extensibility

A problem with this approach is that it is hard to add special cases,
like a more efficient treatment of lists: they could be
length-encoded.

Solution 1: Specialized representation class for each generic
function. 

> class Generic a => GenericList a where

> class RepList a where
>   replist :: (GenericList f) => f a

Solution 2: Abstract over representation *and* the generic function at
the same time using a two-parameter class.

> class GRep f a where
>   grep :: f a

> instance (Generic f) => GRep f Unit where
>   grep = unit

> instance (Generic f, GRep f a, GRep f b) => GRep f (Plus a b) where
>   grep = plus grep grep

| instance GRep ShowBin a => GRep ShowBin [a] where
|   grep = ...

* Abstracting over a type constructor

A function that determines the size of a value is type-generic in a
different way. It needs to be applicable at the following types:

[a] -> Int 
Tree a -> Int

... or generally at type  f a -> Int
where f is a type constructor.

Here is an instance of Generic for counting elements in a container
data structure.

> newtype Count a = Count { count' :: a -> Int }

> instance Generic Count where
>   unit     = Count (const 0)
>   plus a b = Count (\x -> case x of Inl l -> count' a l
>                                     Inr r -> count' b r)
>   pair a b = Count (\x -> count' a (outl x) + count' b (outr x))
>   char     = Count (const 0)
>   int      = Count (const 0)
>   view iso a = Count (\b -> count' a (fromData iso b))

Just using this function generically results in the constant 0
function! To obtain interesting result, we need to provide a
representation for the type constructor.

> size :: FRep h => h a -> Int
> size = count' (frep (Count (const 1)))

> gsum :: FRep h => h Int -> Int
> gsum = count' (frep (Count id))

> class FRep h where
>   frep :: Generic f => f a -> (f (h a))

> instance FRep [] where
>   frep = rList

> instance FRep Tree where
>   frep = rTree

> instance FRep Maybe where
>   frep = rMaybe

** Utility 

> bits n a = let i = fromEnum a in intBits n i
> intBits n j = if n==0 then [] else intBits (n-1) (j `div` 2) ++ [if odd j then I else O]
> type Name = String
> type Arity = Int
