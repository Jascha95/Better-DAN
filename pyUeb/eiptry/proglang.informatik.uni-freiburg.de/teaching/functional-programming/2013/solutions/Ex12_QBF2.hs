{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, DeriveFunctor #-}
module Ex12_QBF2 where

import DTC
import Prelude hiding (or, and)
import Ex12_QBF1

import Test.Framework
import Test.Framework.Providers.QuickCheck2 

data And a = And a a
  deriving Functor
and :: (And :<: f) => Mu f -> Mu f -> Mu f
and e f = inject (And e f)
data QE a = Exists String a
  deriving Functor

exists :: (QE :<: f) => String -> Mu f -> Mu f
exists x phi = inject (Exists x phi)

ex2, ex3, ex4 :: Mu (QA :+: Neg :+: Or :+: BVal :+: Var :+: QE :+: And)
ex2 = forall "x" (neg ((var "x" `or` bval True))) `and` (exists "x" (bval True `and` var "x"))
ex3 = exists "x" (bval True `and` var "x")
ex4 = forall "x" (neg ((var "x" `or` bval True))) `or` (exists "x" (bval True `and` var "x"))

instance SubstEval And where
 sevalAlg (And e f) env = e env && f env
instance SubstEval QE where
 sevalAlg (Exists x e) env = e (benv_cons x True env) || e (benv_cons x False env)

runTests = defaultMain
  [ testProperty "ex2" $ seval_closed ex2 == False
  , testProperty "ex3" $ seval_closed ex3 == True
  , testProperty "ex4" $ seval_closed ex4 == True
  ]
