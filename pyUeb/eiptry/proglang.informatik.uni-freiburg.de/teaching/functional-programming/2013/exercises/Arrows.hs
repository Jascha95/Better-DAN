module Arrows where

import Control.Monad

-- The Arrow class and derived functions:

class Arrow arr where
  arr :: (a -> b) -> arr a b
  first :: arr a b -> arr (a,c) (b,c)
  (>>>) :: arr a b -> arr b c -> arr a c

second :: Arrow arr => arr a b -> arr (c,a) (c,b)
second f = arr swap >>> first f >>> arr swap
  where swap (x,y) = (y,x)

(***) :: Arrow arr => arr a b -> arr c d -> arr (a,c) (b,d)
f *** g = first f >>> second g

(&&&) :: Arrow arr => arr a b -> arr a c -> arr a (b,c)
f &&& g = diag >>> (f *** g)
   where diag :: Arrow arr => arr a (a,a)
         diag = arr (\x -> (x,x))

-- Arrow instances: Functions

instance Arrow (->) where
  arr f = f
  f >>> g = g . f
  first f = \(a,c) -> (f a,c)

-- Arrow instances: Kleisli Arrows

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance (Functor m, Monad m) => Arrow (Kleisli m) where
  arr f = Kleisli (return . f)
  f >>> g = Kleisli (join . fmap (runKleisli g) . runKleisli f)
  first f = Kleisli (\(a, c) -> do { b <- runKleisli f a; return (b,c)})

-- Arrow instances: Stream Transformers/Stream Functions

newtype SF a b = SF { runSF :: [a] -> [b] }

instance Arrow SF where
  arr f = SF (map f)
  f >>> g = SF (runSF f >>> runSF g)
  first f = SF (unzip >>> first (runSF f) >>> uncurry zip)

delay x = SF (init . (x:))

-- Arrow instances: State Transformers
newtype SB s a b = SB { runSB :: (s -> a) -> (s -> b) }
-- ... defining the Arrow instance is left as an exercise


-- Arrows and conditionals: ArrowChoice and derived functions
class Arrow arr => ArrowChoice arr where
  left :: arr a b -> arr (Either a c) (Either b c)

right :: ArrowChoice arr => arr a b -> arr (Either c a) (Either c b)
right f = arr mirror >>> left f >>> arr mirror
  where mirror (Left x) = Right x
        mirror (Right y) = Left y

(+++) :: ArrowChoice arr => arr a b -> arr c d -> arr (Either a c) (Either b d)
f +++ g = left f >>> right g

(|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
f ||| g = f +++ g >>> arr (either id id)

ite :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b
ite p f g = (p &&& arr id) >>> arr isoBoolA >>> (f ||| g)
    where isoBoolA (True, x) = Left x
          isoBoolA (False, x) = Right x

-- Arrow choice instances:
instance ArrowChoice (->) where
  left f = either (Left . f) Right -- Either a c

instance (Monad m, Functor m) => ArrowChoice (Kleisli m) where
  left f = Kleisli (either (liftM Left . runKleisli f) (return . Right))

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
  

-- Towards circuits: ArrowLoop and its instances
class Arrow arr => ArrowLoop arr where
  loop :: arr (a,c) (b,c) -> arr a b

instance ArrowLoop (->) where
  loop f a = b
    where (b,c) = f (a,c)

instance ArrowLoop SF where
  loop f = SF (\as -> let (bs, cs) = unzip (runSF f (zip as (stream cs))) in bs)
    where stream ~(x:xs) = x:stream xs


-- Some fixity definitions taken from Control.Arrow
infixr 1 >>>
infixr 2 +++, |||
infixr 3 ***, &&&

