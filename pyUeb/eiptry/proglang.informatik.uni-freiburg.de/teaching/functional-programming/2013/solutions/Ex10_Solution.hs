module Ex10_Solution where

import Control.Monad
import Data.List (transpose)

import Test.Framework
import Test.Framework.Providers.QuickCheck2  
import Test.QuickCheck

import Arrows

-----------------------
-- Aufgabe 1: filterA
----------------------
filterA :: ArrowChoice arr => arr a Bool -> arr [a] [a]
filterA select = arr listcase >>>
                 arr (const []) ||| (((select &&& arr id) *** filterA select) >>> arr addOrSkip)
  where addOrSkip ((True, x), xs) = x:xs
        addOrSkip ((False, _), xs) = xs

tests_filter =
  testGroup "filterA"
  [ -- filterA should work like filter and filterM
    testProperty "filter even " $
      \xs -> filter even (xs :: [Int]) == filterA even xs
  , testProperty "filterM mEven" $
      \xs b -> filterM (runKleisli $ mEven b) (xs :: [Int]) == runKleisli (filterA (mEven b)) xs
  , testProperty "filterM testA" $
      \xs -> filterM testM (xs :: [Int]) == runKleisli (filterA $ Kleisli testM) xs
  , testProperty "filterM testA, nonnull" $
      \preXs -> let xs = filter (/= 0) preXs
                    spec = filterM testM (xs :: [Int]) 
                in not (null xs) ==>
                   spec == runKleisli (filterA $ Kleisli testM) xs
                   && spec /= Nothing

    -- How does filterA behave for SF?
    --
    -- Without delay, it just filters the lists like (arr (filter f))
  , testProperty "filterA even" $
    \xs -> runSF (filterA (arr even)) (xs :: [[Int]]) == runSF (arr (filter even)) xs
    -- With delay and inputs that have all the same length,
    -- it uses selects elements based on the observations on the previous list:
  , testProperty "filterA even, delay True, same lengths" $
    filter_true_even [[1,2,1,2], [2,1,1,1], [3,5,7,9]]
                     == [[1,2,1,2], [1,1], [3]]

    -- More generally, the selection of the ith element of a list
    -- depends on the last observation for the ith element in prior
    -- stream values:
  , testProperty "filterA even, delay True, different lengths" $
    filter_true_even [ [1,2,1,2]
                     , [2,2]
                     , [3,3,2]
                     , [2,2,3,3]
                     , [2,4,6,8,10,12] ]

    ==               [ [1,2,1,2]
                     , [  2]
                     , [3,3]
                     , [    3,3]
                     , [2,4,    10,12]]
  ]
  where mEven b = if b then Kleisli (Just . even) else Kleisli (const Nothing)
        testM :: Int -> Maybe Bool
        testM x = do
          guard $  x /= 0
          return $ even x

filter_true_even = runSF (filterA (arr even >>> delay True))
filter_false_even = runSF (filterA (arr even >>> delay False))

   -- In summary, the arrow passed to filterA produces a stream of
   -- booleans for each stream of ith elements of the incoming lists;
   -- the ith `column'.  This boolean stream is then used to select
   -- the elements in that column. If a list lacks a column, the
   -- selection arrow simply ignores it.


----------------
-- Aufgabe 2: Counter
----------------
mplex :: SF ((Bool, Int), Int) Int
mplex = arr $ \((b, e), t) -> if not b then t else e -- a three input component
inc :: SF Int Int 
inc = arr (+1)
zInit :: SF a Int
zInit = arr $ const 0

counter :: SF Bool Int
counter =
  (arr id -- boolean input is the first input to mplex
    &&&
  zInit)  -- the constant 0 is the second input 
  >>> 
  loop ( -- the third input comes from a loop
        mplex >>>
        arr id &&& arr id >>> -- duplicate the output of mplex (like the junktion in the diagram)
        second (inc >>> delay 0) -- apply increment and delay to the loop
        )

test_counter = testGroup "Counter"
  [ testProperty "counter example" $
  runSF counter [True, True, False, False, False, True, False, False]
  == [0, 0, 1, 2, 3, 0, 1, 2]
  ]
    

------------------------
-- Aufgabe 3: Moving average
------------------------
  
-- Solution 1: foldSF
foldSF :: (a -> b -> a) -> a -> SF b a
foldSF fld z = SF $ tail . scanl fld z 

sumSF :: Int -> SF Double Double
sumSF n = foldSF (+) 0 >>>     -- sum of values until now
          arr id
           &&&
          (delayN (n) 0) -- sum of values until `n steps ago'
          >>> arr (uncurry (-)) -- substract the current some from that n seconds ago
  where delayN 0 x = arr id
        delayN n' x = delay x >>> delayN (n'-1) x

sma :: Int -> SF Int Double -> SF Int Double
sma n xt = xt >>> sumSF n >>> arr (/ fromIntegral n)

-- Solution 2: Window
windowSF :: a -> Int -> SF a [a]
windowSF dummy width = arr id &&& go (width-1) >>> consA
  where go 0 = arr $ const []
        go n = arr id &&& (go (n-1)) >>> arr (uncurry (:)) >>> mapA (delay dummy)
        consA = arr (uncurry (:))

sumSF' :: Int -> SF Double Double
sumSF' n = windowSF 0 n >>> arr sum

sma' :: Int -> SF Int Double -> SF Int Double
sma' n xt = xt >>> sumSF' n >>> arr (/ fromIntegral n)

series :: [Double]
series = [6, 20, 15, 15, 12, 12, 16, 9, 26, 12, 27, 11, 17, 25, 20, 15, 25, 20, 16, 19]
mkXt :: [Double] -> SF Int Double
mkXt ys = arr $ ((ys ++ repeat 0) !!)

tests_sma =
  testGroup "Moving average"
  [ testProperty "sma series" $ 
      runSF (sma 3 (mkXt series)) [1..20]
      == ex_result
  , testProperty "sma' series" $ 
      runSF (sma' 3 (mkXt series)) [1..20]
      == ex_result
  , testProperty "sma == sma'" $ forAll gen $
     \(n, xs) -> eqSF (sma n $ mkXt xs) (sma' n $ mkXt xs)
  ]
  where eqSF sf1 sf2 = length (runSF sf1 ts) == length (runSF sf2 ts) 
        aEq (x, y) = abs (x - y) < 0.001
        ts = [1..len]
        len = 20 
        gen = do
          n <- choose (3, 5) 
          xs <- vectorOf len (arbitrary :: Gen Double)
          return (n, xs)
        ex_result = [ 6.666666666666667
                    , 11.666666666666666
                    , 16.666666666666668
                    , 14.0
                    , 13.0
                    , 13.333333333333334
                    , 12.333333333333334
                    , 17.0,15.666666666666666
                    , 21.666666666666668
                    , 16.666666666666668
                    , 18.333333333333332
                    , 17.666666666666668
                    , 20.666666666666668
                    , 20.0
                    , 20.0
                    , 20.0
                    , 20.333333333333332
                    , 18.333333333333332
                    , 11.666666666666666 ]

----------------
-- Aufgabe 4
----------------
instance Arrow (SB s) where
  SB g >>> SB f = SB $ f . g 
  arr f = SB $ \st s -> f (st s)
  first (SB f) = SB $ \st -> \ s -> (f (fst . st) s, snd (st s))


-- Interpretation of s: the index into the list. Thus a stream s ::
-- [a] is represented by a function f :: Int -> a where f(i) == s !! i
runSFfromSB :: SB Int a b -> [a] -> [b]
runSFfromSB (SB f) xs = map (f lkp) $ indices
  where lkp i = xs !! i 
        indices = map fst $ zip [0..] xs

-- Example: a SB Version of `delay'
delaySB :: a -> SB Int a a
delaySB n = SB $ \st -> \s -> if s == 0 then n else st (s-1)
        



------------------------
-- Tests
-----------------------
runTests = defaultMain
  [ tests_filter
  , test_counter
  , tests_sma
  ]

