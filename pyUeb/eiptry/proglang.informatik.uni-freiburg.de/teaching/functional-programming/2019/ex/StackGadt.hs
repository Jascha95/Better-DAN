{-#LANGUAGE GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

data Empty = Empty deriving (Show)
data h :! t = h :! t deriving (Show)
infixr :!

data SProg n m where
  Noop :: SProg n n
  Push :: a -> SProg n (a :! n)
  Pop :: SProg (a :! n) n
  Dup :: SProg (a :! n) (a :! a :! n)
  Dup2 :: SProg (a :! b :! n) (a :! b :! a :! b :! n)
  Flip :: SProg (a2 :! a1 :! n) (a1 :! a2 :! n)
  Fun :: (a -> b) -> SProg (a :! n) (b :! n)
  Op :: (a -> b -> c) -> SProg (a :! b :! n) (c :! n)
  If ::  (SProg n m) -> (SProg n m) -> SProg (Bool :! n) m
  While :: SProg n (Bool :! n) -> SProg (Bool :! n) n
  Seq :: SProg n1 n2 -> SProg n2 n3 -> SProg n1 n3

eval :: SProg n m -> n -> m
eval Noop s = s
eval (Push a) s = a :! s
eval Pop (_ :! s) = s
eval Dup (a :! s) = a :! a :! s
eval Dup2 (a :! b :! s) = a :! b :! a :! b :! s
eval Flip (a :! b :! n) = b :! a :! n
eval (Fun f) (a :! n) = f a :! n
eval (Op f) (a :! b :! n) = f a b :! n
eval (If p1 p2) (b :! s) = if b then eval p1 s else eval p2 s
eval (While p) s = eval (If (p ! While p) Noop) s
eval (Seq p1 p2) s = eval p2 $ eval p1 s

infixl 9 !
(!) = Seq

getHead :: a :! n -> a
getHead (a :! _) = a

evalHead p s = getHead $ eval p s -- evaluate A stack program and only return the first element

ge, le :: Ord a => SProg (a :! a :! n) (Bool :! n)
ge = Op (>=)
le = Op (<=)
plus, sub, mult :: Num a => SProg (a :! a :! n) (a :! n)
plus = Op (+)
sub = Op (-)
mult = Op (*)
               
-- replace the two top elements by their maximum
maxStack :: (Ord a0) => SProg (a0 :! a0 :! n) (a0 :! n)
maxStack = Dup2 ! ge ! If (Flip ! Pop) Pop

-- replace (x, y) on top by (x mod y)
modStack :: (Num a, Ord a) => SProg (a :! a :! n) (a :! n)
modStack =
    Dup2 ! ge !
         While (Dup2 ! sub ! Flip ! Pop ! Dup2 ! ge) ! Flip ! Pop

