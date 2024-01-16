module Exercise4 where

import Prelude hiding (foldr, foldl, map, or, and, filter)
import qualified Prelude
import qualified Data.List as List

import Test.QuickCheck
    
-- | 1 : Folding

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (h:t) = f h (foldr f z t)

or = foldr (||) False
and = foldr (&&) True
filter p = foldr cons_if []
    where
      cons_if x l | p x = x:l
                  | otherwise = l
map f = foldr (\x l -> f x : l) []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z l =
    foldr (\x k -> (\acc -> k (f acc x))) id l z

remdups l0 =
    foldr (\x l -> if x `elem` l then l else x:l) [] l0

propOr,propAnd :: [Bool] -> Bool
propOr l = or l == List.or l
propAnd l = and l == List.and l

propFilter :: (Fun Int Bool) -> [Int] -> Bool
propFilter f l =
    filter (applyFun f) l == List.filter (applyFun f) l

propMap :: (Fun Int Int) -> [Int] -> Bool
propMap f l =
    map (applyFun f) l == List.map (applyFun f) l

propFoldl :: (Fun (Int, Float) Int) -> Int -> [Float] -> Bool
propFoldl f z l =
    foldl (applyFun2 f) z l == List.foldl (applyFun2 f) z l

propDup :: [Int] -> Bool
propDup l = remdups l == List.nub l

-- | 2 : Unfolding

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f z = case f z of 
                Nothing -> []
                Just (h, z') -> h : unfoldr f z'

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfoldr uncons
    where
      uncons [] = Nothing
      uncons (h:t) = Just (f h, t)

iterate :: (a -> a) -> a -> [a]
iterate f x = unfoldr (\x -> Just (x, f x)) x


-- | 3 : Hamming

hamming :: (Num a, Ord a) => [a]
hamming =
    1 :
      map (*2) hamming `merge`
      map (*3) hamming `merge`
      map (*5) hamming

merge [] l = l
merge l [] = l
merge l1@(x1:t1) l2@(x2:t2) = case compare x1 x2 of
                                LT -> x1 : merge t1 l2
                                EQ -> x1 : merge t1 t2
                                GT -> x2 : merge l1 t2
