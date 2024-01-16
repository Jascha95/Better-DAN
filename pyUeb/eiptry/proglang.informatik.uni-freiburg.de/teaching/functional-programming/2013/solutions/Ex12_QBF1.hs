{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, DeriveFunctor #-}
module Ex12_QBF1 where
import DTC
import Prelude hiding (or)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 

-------------------------
-- Aufgabe 1.1: Funktoren für Konstanten, Konjunktion, Negation und
-- All-Quantifikation
-------------------------
data BVal a = BVal Bool
  deriving Functor
data Or a = Or a a
  deriving Functor
data Neg a = Neg a
  deriving Functor

bval :: (BVal :<: f) => Bool -> Mu f 
bval b = inject (BVal b)

or :: (Or :<: f) => Mu f -> Mu f -> Mu f
or x y = inject (Or x y)

neg :: (Neg :<: f) => Mu f -> Mu f
neg x = inject (Neg x)

type Id = String
data Var a = Var Id
  deriving Functor

data QA a = Forall String a 
  deriving Functor

forall :: (QA :<: f) => String -> Mu f -> Mu f
forall v phi = inject (Forall v phi)

var :: (Var :<: f) => Id -> Mu f
var i = inject (Var i)

-- Beispiel vom Blatt:
ex1 :: (QA :<: f, Neg :<: f, Or :<: f, BVal :<: f, Var :<: f)
       => Mu f
ex1 = forall "x" (neg (or (var "x") (bval True)))

-- alternative Typsignatur; allerdings ist ex1' so nicht mehr mit
-- größeren Funktoren verwendbar.
ex1' :: Mu (QA :+: Neg :+: Or :+: BVal :+: Var)
ex1' = ex1
    

---------------------------
-- Aufgabe 1.2: Auswertung
---------------------------
-- Kodierung der Variablenbelegung als Funktion
-- (Alternative: Assoziationsliste, [(String, Bool)], oder Data.Map)
type BEnv = String -> Bool
benv_empty = const $ error "not found" 
benv_cons s b env s' | s == s' = b 
                     | otherwise = env s
-- Ein boolescher Wert, der von einer Variablenbelegung abhängt:
type EBool = BEnv -> Bool

class SubstEval f where
  sevalAlg :: f EBool -> EBool

-- Immer notwendig: Instanz für die Kombination von Funktoren:
instance (SubstEval f, SubstEval g) => SubstEval (f :+: g) where
  sevalAlg (Inl f) = sevalAlg f
  sevalAlg (Inr g) = sevalAlg g

instance SubstEval BVal where
  sevalAlg (BVal b) env = b
          
instance SubstEval Var where
  sevalAlg (Var i) env = env i

instance SubstEval QA where 
  sevalAlg (Forall x phi) env = phi (benv_cons x True env)
                                && 
                                phi (benv_cons x False env)
instance SubstEval Or where
  sevalAlg (Or b1 b2) env = b1 env || b2 env

instance SubstEval Neg where
  sevalAlg (Neg b) env = not (b env)

seval t env = (foldMu sevalAlg t) env
seval_closed t = seval t benv_empty
-- TODO: open recursion version
-- TODO: try to use constraint kinds

runTests = defaultMain
  [ testProperty "ex1" $ seval_closed ex1' == False ] 



