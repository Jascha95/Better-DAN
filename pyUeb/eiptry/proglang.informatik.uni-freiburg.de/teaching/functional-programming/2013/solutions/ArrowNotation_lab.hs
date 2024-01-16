{-# LANGUAGE Arrows #-}
module ArrowNotation where

-- Achtung: die Arrow Notation funktioniert nur mit den Arrow-Klassen
-- aus der Standardbibliothek: Control.Arrow!

import ArrowsStd

-- Remember mulA from the lecture?
-- It is a little inconvenient to write.
-- f(x) * g(x)
mulA :: Arrow arr => arr a Int -> arr a Int -> arr a Int
mulA f g = f &&& g >>> arr (uncurry (*))

-- The following arrow even more so:
max3A :: Arrow arr => arr a Int -> arr a Int -> arr a Int -> arr a Int
max3A f g h = f &&& g &&& h >>> second (arr (uncurry max)) >>> arr (uncurry max)

-- Arrow Notations to the rescue:
max3A' :: Arrow arr => arr a Int -> arr a Int -> arr a Int -> arr a Int
max3A' f g h = proc x -> do
  xf <- f -< x
  xg <- g -< x
  xh <- h -< x
  returnA -< max xf (max xg xh)

-- A little more concise(Petricheck, Orchard, Mycroft 2013)
max3A'' :: Arrow arr => arr a Int -> arr a Int -> arr a Int -> arr a Int
max3A'' f g h = proc x' -> do
  (x, (y, z)) <- f &&& g &&& h -< x'
  returnA -< max x (max y z)

-- Intuitively: tuple the inputs correctly and use first/second appropiately
-- Details: http://www.haskell.org/ghc/docs/papers/arrow-rules.pdf

-- A bigger example: (x - min xs) / (max xs - min xs)
normalizeA :: Arrow arr => arr Double Double -> arr Double Double -> arr Double Double
normalizeA minA maxA =
             (arr id &&& minA >>> arr (\(x, m) -> x - m))
             &&&
             ((maxA &&& minA) >>> arr (uncurry (-)))
             >>> arr (uncurry (/))

normalizeA' :: Arrow arr => arr Double Double -> arr Double Double -> arr Double Double
normalizeA' minA maxA = proc x -> do 
  mi <- minA -< x
  ma <- maxA -< x
  returnA -< (x - mi) / (ma - mi)

-- More sophisticated features:

flipflop = proc (reset, set) -> do
  rec c <- delay False <<< nor -< (reset, d)
      d <- delay True <<< nor -< (set, c)
  returnA -< (c,d)
  where nor :: SF (Bool, Bool) Bool
        nor = proc (x, y) -> returnA -< not (x || y)

  -- früher:
  -- loop (arr (\((reset,set),~(c,d)) -> ((set,d),(reset,c))) >>>
  --       (nor *** nor) >>>
  --       delay (False,True) >>>
  --       (arr id &&& arr id))



mapA' :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA' f = proc xs -> do
   case xs of
     [] ->    returnA -< []
     x:xs' -> do
       x' <- f -< x
       xs'' <- mapA f -< xs'
       returnA -< x':xs''

  -- früher: siehe ArrowStd.hs


      
      
mapSF f = SF $ \xs -> f xs
minSF,maxSF :: Ord a => SF a a
minSF = mapSF (\xs -> map (const $ minimum xs) xs)
maxSF = mapSF (\ys -> map (const $ maximum ys) ys)
