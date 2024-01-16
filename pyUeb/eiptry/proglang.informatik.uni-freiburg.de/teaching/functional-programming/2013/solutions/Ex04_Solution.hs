-- Korrigierte Version: 2013-12-03
module Ex04_Solution where

import Prelude hiding (or, filter, map, foldl, iterate, until)
import Data.Char
import qualified Data.List as L
import qualified Prelude as P
import System.IO

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Ex01_Solution

-- |---------------------|
-- | foldr
-- |---------------------|
or :: [Bool] -> Bool
or = foldr (||) False

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (select) []
  where select x acc | f x       = x:acc
        select _ acc | otherwise = acc

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = foldr build id xs z 
  where build x acc = acc . \y -> f y x

foldl_cheat f z = foldr appToRest z . reverse
  where appToRest x acc = f acc x  

remdups :: Eq a => [a] -> [a]
remdups = foldr select []
  where select x res@(y:_) | x == y  = res
        select x res       | otherwise = x:res

tests_foldr =
  testGroup "foldr"
  [ testProperty "or" $ \xs -> or xs == P.or xs
  , testProperty "filter" $ \xs -> filter isUpper xs == L.filter isUpper xs
  , testProperty "map" $ \xs -> map toUpper xs == L.map toUpper xs
  , testProperty "foldl" $ \xs -> foldl_cheat (flip (:)) [] (xs :: [Int]) == reverse xs
  , testProperty "foldl2" $
    \xs -> let f (base, v) x = (base*10,base * (boolToInt x) + v)
               boolToInt True = 1
               boolToInt False = 0
               z = (1, 0)
           in foldl_cheat f z xs == L.foldl' f z xs
  , testProperty "foldl3" $
    \xs -> let f (base, v) x = (base*10,base * (boolToInt x) + v)
               boolToInt True = 1
               boolToInt False = 0
               z = (1, 0)
           in foldl f z xs == L.foldl' f z xs
  , testProperty "remdups" $
    \(Ordered xs) -> remdups (xs :: [Int]) == L.nub xs
  , testProperty "remdups2" $
    \(Dups xs) -> let xs' = L.sort xs in remdups (xs') == L.nub xs'
  , testProperty "remdups3" $
    \(Dups xs) ->
    let xs' = L.sort xs :: [Int]
    in (not $ null xs') && (not $ head xs' == last xs')
       ==> remdups (xs' ++ xs') == (L.nub xs') ++ (L.nub $ xs')

  ] 

newtype Duplicates = Dups [Int]
  deriving Show

instance Arbitrary Duplicates where
  arbitrary = do
     ls <- listOf (listOf . return =<< arbitrary)
     return $ Dups $ concat ls
    
-- |---------------------|
-- | Unfoldr 
-- |---------------------|

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f seed = go (f seed)
  where go Nothing         = []
        go (Just (x, acc)) = x : go (f acc)

map_unfoldr :: (a -> b) -> [a] -> [b]
map_unfoldr f = unfoldr next
  where next [] = Nothing
        next (x:xs) = Just (f x, xs)

iterate :: (a -> a) -> a -> [a]
iterate f = unfoldr $ \x -> Just (x, f x)

tests_unfoldr =
  testGroup "unfoldr"
  [ testProperty "map" $
    \xs -> map_unfoldr toUpper xs == map toUpper xs
  , testProperty "iterate" $
    \z n -> let f x = n * x :: Int
            in take 100 (iterate f z) == take 100 (L.iterate f z)
  ]

-- |---------------------|
-- | Wahrscheinlichkeiten
-- |---------------------|

tries _ acceptable prob | not (valid acceptable) 
                          ||
                          not (valid prob) = error "Invalid probabilities"
  where valid n = abs n <= 1
tries countRelevant acceptable prob = countRelevant acceptable
                                      $ scanl (*) prob
                                      $ repeat prob

until acc = (+1) . before acc
before acc = length . takeWhile (>= acc)

triesWithAcceptableRisc = lmap2 invert (tries before) `on` fromPercent 

iterationsForAcceptableYield = tries until `on` (invert . fromPercent)

fromPercent :: Int -> Double
fromPercent n = fromIntegral n / 100

invert :: Double -> Double
invert prob = 1 - prob
              
-- Helper combinators

-- modify the first argument of a function
lmap1 = flip (.)
-- modify the second argument of a function
lmap2 g f x = lmap1 g $ f x

-- modify both argument of a function
-- also provided by Data.Function
on f g = lmap1 g $ lmap2 g $ f

tests_prob =
  testGroup "prob"
  [ testProperty "bahnfahrten" $ triesWithAcceptableRisc 75 10 == 2
  , testProperty "korrektur" $ iterationsForAcceptableYield 99 80 == 3
  ]

-- |---------------------|
-- | Binary
-- |---------------------|

-- Produce a 2-Komplement, little endian encoding of an Integer, with
-- a fixed word size. The least-significant bit ist first
type Bit = Bool
type Word = [Bit]

data Sign = Pos | Negative
  deriving Show
sgn :: Integer -> Sign
sgn n | n < 0     = Negative
sgn _ | otherwise = Pos

encode :: Int -> Integer -> [Word]
encode wordSize n = L.genericTake neededWords
                    $ splitEvery wordSize
                    $ twoComplement (sgn n)
                    $ bits n
  where neededWords =  head $ dropWhile (not . fits) $ [1..]
        fits wCount = n >= 0 && abs n < 2^(wbits-1)
                    || n < 0  && n >= (-1) * 2^(wbits-1)
           where wbits = wCount * wordSize

-- We have to check, if the list of list of bits are words of correct
-- length. Also, the empty list does not make sense as a number.
decode n ws@(_:_) | all ((== n) . length) ws = Just $ decodeRaw ws
decode _ _        | otherwise                = Nothing

-- Real decoding
decodeRaw ws = adjustSgn wSgn $ sum $ zipWith (*) (L.iterate (*2) 1)
               $ L.map bitToInt
               $ twoComplement wSgn $ concat ws
  where wSgn = if last (last ws) then Negative else Pos
        bitToInt True = 1
        bitToInt _    = 0

        adjustSgn Pos = id
        adjustSgn Negative = (* (-1))
        
splitEvery :: Int -> [a] -> [[a]]
splitEvery n | n <= 0    = error "negative split number!"
splitEvery n | otherwise = foldr wrap [] . zip [1..]
  where wrap (i, x) acc     | i `mod` n == 0
                              || null acc    = [x]:acc 
        wrap (_, x) (a:acc) | otherwise      = (x:a):acc
        wrap (_, x)  _                       = error "impossible case"


twoComplement :: Sign -> [Bit] -> [Bit]
twoComplement Pos = (++ [False])
twoComplement Negative = binInc . L.map not 
  
-- infinite lists of bits, encoding n (lsb first)
bits :: Integer -> [Bit]
bits n = L.map lsb $ L.iterate (`div` 2) (abs n)

-- calculate the least significant bit of an Integer
lsb :: Integer -> Bit
lsb = not . even -- (== 1) . (`mod` 2)

-- perform arithmetic incrementation on a Bitstring
binInc :: [Bit] -> [Bit]
binInc = go True
  where go False [] = []
        go True  [] = [True]
        go carry (x:xs) = x `xor` carry : go (x && carry) xs

        x `xor` y = x && not y || y && not x


tests_binary =
  testGroup "binary"
  [ testProperty "encode -> decode" $
    \(WSize wsize) n -> (decode wsize . encode wsize) n == Just n
  , testProperty "decode -> encode" $
    \(Words wsize ws) ->
    maybe
      False
      (`L.isPrefixOf` ws)
      $ (maybe Nothing (Just . encode wsize) . decode wsize) ws 
  ]

newtype WordSize = WSize Int
  deriving Show

instance Arbitrary WordSize where
  arbitrary =
    (\(Positive i) -> return $ WSize i) =<< resize 17 arbitrary
  
data Words = Words Int [Word]
  deriving Show

instance Arbitrary Words where
  arbitrary = do
    WSize wSize <- arbitrary
    ws <- resize 10 $ listOf1 $ vectorOf wSize $ arbitrary
    return $ Words wSize ws



  
-- |-----------------------|
-- | readLine              |
-- | ----------------------|

readLine :: IO String
readLine = do
  -- hSetBuffering stdin NoBuffering
  -- hSetBuffering stdout NoBuffering
  -- hSetEcho stdin False
  readLine' ""
  where readLine' text = do
          let l = length text
          putStr $ "\CR" ++ text
          c <- getChar
          putStr $ "\CR" ++ replicate l ' ' 
          case c of
            '\n' -> putStr "\n" >> return text
            '\DEL' -> readLine' $ take (l - 1) text
            _    -> readLine' $ text ++ [c]




-- |-----------------------|
-- | Calculator Interface
-- | ----------------------|

type Stack = [Int]

main :: IO ()
main = runCalc [] 

-- not the use of `maybe' (if not yet known)
-- Defined in Prelude:
-- maybe :: b -> (a -> b) -> Maybe a -> b
runCalc st = do
  printStack st >> readLine >>= interpret st >>= maybe (return ()) runCalc

interpret st cmd | validCmd cmd = return $ Just $ readCommand cmd st
interpret st cmd | cmd == "exit" = do
  putStrLn "Good Bye!"
  return Nothing
interpret st cmd  | otherwise = do
  putStrLn $ "Error: illegal command `" ++ cmd ++ "'"
  return $ Just st

printStack st = putStrLn $ show st

validCmd cmd = cmd `elem` ["add", "substract", "pop" ] || isInt cmd


       
-- |-----------------------|
-- | Test runner
-- | ----------------------|
runTests :: IO ()
runTests =
  defaultMain
  [ tests_foldr
  , tests_unfoldr
  , tests_prob
  , tests_binary
  ]
