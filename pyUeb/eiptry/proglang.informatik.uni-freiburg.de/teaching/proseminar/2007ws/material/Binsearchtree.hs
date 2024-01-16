module Binsearchtree where
 import Prelude

 data Tree elt = E | T (Tree elt) elt (Tree elt) deriving Show

 empty :: Tree elt
 empty = E

 member :: Ord elt => elt -> Tree elt -> Bool
 member x E = False
 member x (T a y b) 	| x < y 	= member x a
			| x==y		= True
			| x > y		= member x b

 insert :: Ord elt => elt -> Tree elt -> Tree elt
 insert x E = (T E x E)
 insert x (T a y b) 	| x < y 	= T (insert x a) y b
			| x == y	= T a y b
			| x > y 	= T a y (insert x b)

