module ArrowsStd
       ( module Control.Arrow
       , SF(..)
       , delay
       , SB(..)
       , ite
       , listcase
       , mapA
       )
       where
import Prelude hiding ((.), id)

import Control.Monad
import Control.Category 
import Control.Arrow


-- Arrow instances: Stream Transformers/Stream Functions

newtype SF a b = SF { runSF :: [a] -> [b] }

instance Category SF where
  id = SF id
  g . f = SF (runSF f >>> runSF g)
         
instance Arrow SF where
  arr f = SF (map f)
  first f = SF (unzip >>> first (runSF f) >>> uncurry zip)

delay x = SF (init . (x:))

-- Arrow instances: State Transformers
newtype SB s a b = SB { runSB :: (s -> a) -> (s -> b) }
-- ... defining the Arrow instance is left as an exercise


-- Arrows and conditionals: Derived functions

ite :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b
ite p f g = (p &&& arr id) >>> arr isoBoolA >>> (f ||| g)
    where isoBoolA (True, x) = Left x
          isoBoolA (False, x) = Right x

-- Arrow choice instances:

instance ArrowChoice SF where
  left f' = SF (weave (runSF f'))
    where weave :: ([a] -> [b]) -> ([Either a c] -> [Either b c])
          weave f xs = let fxs = f (foldr g [] xs)
                           g (Left a) = (a :)
                           g (Right c) = id
                           h [] _ = []
                           h (Left a: rest) (fa: fas) = Left fa: h rest fas
                           h (Right c: rest) fas = Right c: h rest fas
                           h (Left _ : _) [] = error "weave: impossible case"
                       in  h xs fxs

-- Using ArrowChoice: mapA
listcase :: [a] -> Either () (a, [a]) 
listcase [] = Left ()
listcase (x:xs) = Right (x, xs)

mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>>
         arr (const []) ||| ((f *** mapA f) >>> arr (uncurry (:)))
  

-- Towards circuits: ArrowLoop instances

instance ArrowLoop SF where
  loop f = SF (\as -> let (bs, cs) = unzip (runSF f (zip as (stream cs))) in bs)
    where stream ~(x:xs) = x:stream xs


