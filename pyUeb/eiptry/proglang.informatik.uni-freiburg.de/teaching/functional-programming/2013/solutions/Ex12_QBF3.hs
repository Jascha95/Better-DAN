{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, DeriveFunctor #-}
module Ex12_QBF3 where
import DTC
import Prelude hiding (or)
import Ex12_QBF1
import Ex12_QBF2

-- a pretty printer
-- TODO: mention the difference between open recursion and algebra 
class RenderB f where
  renderAlg :: f String -> String 

instance (RenderB f, RenderB g) => RenderB (f :+: g) where
  renderAlg (Inl f) = renderAlg f
  renderAlg (Inr g) = renderAlg g

instance RenderB BVal where
  renderAlg (BVal b) = show b

instance RenderB Var where
  renderAlg (Var i) = i

instance RenderB Neg where
  renderAlg (Neg f) = "¬" ++ f 

instance RenderB Or where
  renderAlg (Or f g) = parentize (f ++ " ∨ " ++ g)

instance RenderB QA where
  renderAlg (Forall v phi) = parentize ("∀" ++ v ++ ". " ++ phi)

instance RenderB QE where
  renderAlg (Exists x phi) = parentize ("∃" ++ x ++ ". " ++ phi)
instance RenderB And where
  renderAlg (And e f) = parentize (e ++ " ∧ " ++ f)

renderB :: (Functor f, RenderB f) => Mu f -> String
renderB tm = foldMu renderAlg tm

parentize :: String -> String
parentize s = "(" ++ s ++ ")"
   

