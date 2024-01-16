module Trie where

import qualified Data.Map as Map

data Trie a = Trie Bool (Map.Map a (Trie a))

empty :: Trie a
empty = Trie False Map.empty

singleton :: Ord a => [a] -> Trie a
singleton [] = Trie True Map.empty
singleton (c : t) =
    Trie False (Map.singleton c (singleton t))

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ m) = Trie True m
insert (c:l) (Trie b m) =
    case Map.lookup c m of
      Just t -> Trie b (Map.insert c (insert l t) m)
      Nothing -> Trie b (Map.insert c (singleton l) m)

member :: Ord a => [a] -> Trie a -> Bool
member [] (Trie b _) = b
member (c:l) (Trie _ m) =
    case Map.lookup c m of
      Just t -> member l t
      Nothing -> False

prefix :: Ord a => [a] -> Trie a -> Trie a
prefix [] t = t
prefix (c:l) t = Trie False (Map.singleton c (prefix l t))

union :: Ord a => Trie a -> Trie a -> Trie a
union (Trie b1 m1) (Trie b2 m2) =
    Trie b m
    where
      b = b1 && b2
      m = Map.unionWith union m1 m2

ofList :: Ord a => [[a]] -> Trie a
ofList l = foldr insert empty l
