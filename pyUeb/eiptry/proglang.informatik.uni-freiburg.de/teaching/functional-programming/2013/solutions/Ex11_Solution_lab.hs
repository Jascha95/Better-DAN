{-# LANGUAGE GADTs #-}
module Ex11_Solution_lab where

import Data.Monoid
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Time.Clock

import Robot

-- Aufgabe 1: SafeList
data Empty 
data NonEmpty
data Unknown

data SafeList state a where
  Nil :: SafeList Empty a
  Cons :: a -> SafeList state a -> SafeList NonEmpty a
  SomeList :: SafeList state a -> SafeList Unknown a

fromSomeList :: SafeList state a -> Either (SafeList Empty a) (SafeList NonEmpty a)
fromSomeList (Nil) = Left Nil
fromSomeList l@(Cons _ _) = Right l
fromSomeList (SomeList l) = fromSomeList l

instance Show a => Show (SafeList state a) where
  show (Nil) = "Nil"
  show (Cons x xs) = "(Cons " ++ show x ++ " " ++ show xs ++ ")"
  show (SomeList l) = show l 

safeHead :: SafeList NonEmpty a -> a 
safeHead (Cons x xs) = x

ex_safeList1 = Cons 5 (Cons 6 Nil) 


safeDrop :: Int -> SafeList state a -> Either (SafeList Empty a) (SafeList NonEmpty a)
safeDrop n Nil = Left Nil
safeDrop 0 l@(Cons _ _) = Right l
safeDrop n (Cons _ xs) = safeDrop (n-1) xs
safeDrop n (SomeList l) = safeDrop n l

safeDrop' :: Int -> SafeList state a -> SafeList Unknown a
safeDrop' n Nil = SomeList Nil
safeDrop' 0 l@(Cons _ _) = SomeList l
safeDrop' n (Cons _ xs) = safeDrop' (n-1) xs
safeDrop' n (SomeList l) = safeDrop' n l

safeAppend :: SafeList state a -> SafeList NonEmpty a -> SafeList NonEmpty a
safeAppend Nil xs = xs
safeAppend (Cons x xs) ys = Cons x $ safeAppend xs ys
safeAppend (SomeList l) ys = safeAppend l ys

data Prog a b where
  Noop :: Prog a a
  Pop     :: Prog (b, a) a
  Push    :: b -> Prog a (b, a)
  Dup     :: Prog (a, b) (a, (a, b))
  Dup2     :: Prog (a, (b, c)) (a, (b, (a, (b, c))))
  Flip :: Prog (a, (b, c)) (b, (a, c))
  AOp2  :: (Int -> Int -> Int) -> Prog (Int ,(Int, d)) (Int, d)
  AOp1 :: (Int -> Int) -> Prog (Int, d) (Int, d)
  BOp1 :: (Bool -> Bool) -> Prog (Bool, d) (Bool, d)
  ABOp2 :: (Int -> Int -> Bool) -> Prog (Int ,(Int, d)) (Bool, d)
  Seq     :: Prog a b -> Prog b c -> Prog a c
  If :: Prog a b -> Prog a b -> Prog (Bool, a) b
  While :: Prog a (Bool, a) -> Prog (Bool, a) a

-- Implementieren Sie einen Interpreter für Prog
eval :: Prog a b -> a -> b
eval (Pop) (i, s) = s
eval (Push i) s = (i, s)
eval Dup (x, s) = (x, (x, s))
eval Dup2 (x, (y, s)) = (x, (y, (x, (y, s))))
eval Flip (x, (y, s)) = (y, (x, s))
eval (AOp2 f) (i, (j, s)) = (f i j, s)
eval (BOp1 f) (i, s) = (f i , s)
eval (AOp1 f) (b, s) = (f b , s)
eval (ABOp2 f) (i, (j, s)) = (f i j, s)
eval (Seq p p') s = eval p' . eval p $ s
eval (If t e) (b, s) = if b then eval t s else eval e s
eval (While p) (b, s) = if b then eval (p <+> While p) s else s
eval (Noop) s = s

infixl 4 <+> 
(<+>) :: Prog a b -> Prog b c -> Prog a c
(<+>) = Seq

-- Schreiben Sie ein Prog-Programm, dass den Betrag des obersten Stack-Elements berechnet
-- Schreiben Sie 2 weitere Prog-Bespielprogramme und testen Sie diese.
ex_p1 = Push 8 <+> Dup <+> AOp2 (*)
ex_p2 = Dup <+> Push 0 <+> ABOp2 (>=) <+> If (AOp1 negate) Noop
ex_p3 = Push 0 <+> ABOp2 (>=) <+> If (AOp1 negate) Noop

ex_eval1 = eval (ex_p1  <+> AOp1 negate <+> ex_p2) ()
-- TODO: eval2 is nonsense
ex_eval2 = eval (ex_p1  <+> AOp1 negate <+> Push 1 <+> ex_p3) ()

-- Fügen Sie nun das Schleifenkonstrukt `while' zu Prog hinzu.
--  Schreiben Sie ein Prog-Program, welches gegeben x y das Ergebnis x
--  `mod` y berechnet (falls y < 0 darf sich das Program beliebig
--  verhalten). Benutzen Sie dafür nur die Operationen aus Fig.1 (und while, natürlich)
substract = AOp2 (-)
ge = ABOp2 (>=)
le = ABOp2 (<=)
notP = BOp1 not
lt = ge <+> notP

ex_div =  Dup2
       <+> substract
       <+> Push 0
       <+> le
       <+> While (
          Dup2
          <+> substract -- [x-z,x,z]
          <+> Flip -- [x, x-z,z]
          <+> Pop -- [x-z,z]
          <+> Dup2 -- [x-z, z, x-z,z]
          <+> substract -- [x-z-z,x-z,z]
          <+> Push 0 -- 
          <+> le
          )
       <+> Flip
       <+> Pop
-- Geben Sie ein Beispiel eines prinzipiell fehlerfreien Programs
-- Stack-Programms, dass Sie /nicht/ mit ihrem Prog Datentyp ausdrücken
-- können.

ex_dontwork1 = undefined -- le <+> notP <+> While (Push 1 <+> Flip)
ex_dontwork2 = undefined -- Push 0 <+> Push 1 <+> Push False <+> If (notP) (substract)


-- ROBOT:

-- Dieser Datentyp unterscheidet Mining und andere Robot-Aktionen Um
-- go zu unteststüzten, speichert AMining auc den go-Wert ab. Er kann
-- so noch modifiziert werden, bevor er ausgeführt wird.
data ARobot b where
  AMining :: Int -> Robot Gold -> ARobot Gold
  AOther :: Robot b -> ARobot b

-- Diese Liste sammelt ARobot-Aktionen. Für nebeneinanderliegende
-- ARobots ist es möglich festzustellen, ob Sie Gold produzieren,
-- oder nicht
data RobotG' b where
  Singleton :: ARobot a -> RobotG' a
  Cons' :: ARobot a -> RobotG' b -> RobotG' b

-- Die neuen ,,atomaren'' Robot-Aktionen sind Singletons der
-- entsprechenden ARobot-Aktionen
mine' = Singleton (AMining 0 mine)
scan' = Singleton (AOther scan)

-- Go' wendet sich bei Mining-Aktionen noch nicht sofort an. Das
-- erlaubt das Verändern des Weges bei Kombintionen.
go' i (Singleton gc) = Singleton (goARobot i gc)
go' i (Cons' gc rest) = Cons' (goARobot i gc) (go' i rest) 

goARobot i (AMining j c) = AMining (i+j) c
goARobot i (AOther c) = AOther (go i c)

-- Hier wird die Kombination definierit. Wenn zwei Miniung Aktionen
-- zusammentreffen, werden diese mit >+> kombiniert. Die go-Werte
-- werden zusammengefasst.
combineARobot (AMining j c) (AMining i c') = Singleton (AMining j (c >+> go (i-j) c'))
combineARobot gc1 gc2 = Singleton (AOther (runARobot gc1 <-> runARobot gc2))

(<+++>) :: RobotG' a -> RobotG' b -> RobotG' b
Singleton gc1   <+++> Singleton gc2        = combineARobot gc1 gc2
Singleton gc1 <+++> Cons' gc2 rest = combineARobot gc1 gc2 <+++> rest
Cons' gc1 rest <+++> rs = Singleton gc1 <+++> (rest <+++> rs)

-- Ausführen eines ARobots
runARobot :: ARobot b -> Robot b
runARobot (AMining i c) = go i c
runARobot (AOther c) = c

-- Ausführen einer RobotG' Aktion
runRobotG' :: RobotG' b -> Robot b
runRobotG' (Singleton gc) = runARobot gc
runRobotG' (Cons' gc rest) = runARobot gc <-> runRobotG' rest


-- Bespielprogramme
robprogG = (go' 7 mine') <+++> (go' 10 mine') <+++>  (go' 6 scan')
someminingG = go' 7 (mine' <+++> go' (-8) mine')
someminingG' = go' 7 (mine' <+++> go' (8) mine')
sm1 = scan' <+++> mine' <+++> mine' <+++> scan'


