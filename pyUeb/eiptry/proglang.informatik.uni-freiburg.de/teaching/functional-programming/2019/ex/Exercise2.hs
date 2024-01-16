module Exercise02 where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map
import Test.QuickCheck

-- | Ex 1  Fib
    
fib, fibFast :: Integer -> Integer

fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)
        
fibFast n =
    fibList !! fromInteger n
    where
      fibList = 0 : 1 : zipWith (+) fibList (tail fibList)


boundedInt :: Integer -> Gen Integer
boundedInt n = suchThat arbitrary (\x -> x > 0 && x < n)

propFib =
    forAll (boundedInt 30) (\x -> fib x == fibFast x)




           


           


           
-- | Ex 2  Undup

undup :: Eq a => [a] -> [a]
undup [] = []
undup (x:xs) = x : (undup $ filter (/= x) xs)

propUndup :: [Int] -> Bool
propUndup l = undup l == List.nub l






















              
-- | Ex 3  Smallest factor

isFactor n x = (n `mod` x) == 0

smallestFactorRec :: Integer -> Integer
smallestFactorRec n =
    f 2
    where
      f x | x * x > n = n
          | isFactor n x = x
          | otherwise    = f (x + 1)

primes = sieve [2..] 
sieve (p:ps) =
    p : sieve [ x | x <- ps, not $ isFactor x p ]
               
smallestFactorSieve 1 = 1
smallestFactorSieve n =
    Maybe.fromJust $ List.find (isFactor n) primes

smallestFactorList 1 = 1
smallestFactorList n =
    head (filter (isFactor n) [2..n])
         
propFactor =
    forAll (boundedInt 1000)
               (\x ->
                    let a = smallestFactorSieve x in
                    let b = smallestFactorRec x in
                    let c = smallestFactorList x in
                    a == b && b == c)

















           
-- | Ex 4  Media Library

type Rating = Integer
type Artist = String
type Length = Float
type User = Int

data Track = Track Artist [(User,Rating)] Length
type Album = String
data MediaBib = MediaBib (Data.Map.Map Album [Track])

addTrack t a (MediaBib b) =
    if Data.Map.member a b then
        MediaBib $ Data.Map.update (\l -> Just (t:l)) a b
    else
        MediaBib $ Data.Map.insert a [t] b







                 
-- | Ex 5  Tic-Tac-Toe

data Field = X | O | E deriving (Eq)
type Board = [[Field]]
    
check b =
    List.length b == 3
            && List.all (\x -> List.length x == 3) b

get :: Board -> (Int, Int) -> Field
get b (i, j) = b !! i !! j

l = [1,2,3]
linesB, columnsB, diagonalsB, allB :: [[(Int,Int)]]
linesB = fmap (\x -> fmap (\y -> (x,y)) l) l
columnsB = fmap (fmap $ \(a,b) -> (b,a)) linesB
diagonalsB =
    [zipWith (,) l l, zipWith (,) l (List.reverse l)]

allB = linesB ++ columnsB ++ diagonalsB

win o b =
    List.any test allB
    where
      test :: [(Int,Int)] -> Bool
      test l =
          let l' = fmap (get b) l in
          List.all (== o) l'
