module TrieMap where

import Prelude hiding (lookup)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Data.Map as Map
import Test.QuickCheck

data Trie a b = Trie (Maybe b) (Map.Map a (Trie a b))

instance (Show a, Show b) => Show (Trie a b) where
    show (Trie v children) =
        prefix ++ Map.foldrWithKey f "" children
        where
          prefix = case v of
                     Just v -> show v
                     Nothing -> ""
          f k v former =
              show k ++ "→ [" ++ show v ++ "]" ++ former
            
empty :: Trie a b
empty = Trie Nothing Map.empty

singleton :: Ord a => [a] -> b -> Trie a b
singleton l b = prefix l (Trie (Just b) Map.empty)

insert :: Ord a => [a] -> b -> Trie a b -> Trie a b
insert [] v (Trie _ m) = Trie (Just v) m
insert (c:l) v (Trie b m) =
    case Map.lookup c m of
      Just t -> Trie b (Map.insert c (insert l v t) m)
      Nothing -> Trie b (Map.insert c (singleton l v) m)

lookup :: Ord a => [a] -> Trie a b -> Maybe b
lookup [] (Trie v _) = v
lookup (c:l) (Trie _ m) =
    case Map.lookup c m of
      Just t -> lookup l t
      Nothing -> Nothing

member :: Ord a => [a] -> Trie a b -> Bool
member k t = Maybe.isJust $ lookup k t


prefix :: Ord a => [a] -> Trie a b -> Trie a b
prefix [] t = t
prefix (c:l) t = Trie Nothing (Map.singleton c (prefix l t))

union :: Ord a => (b -> b -> b) -> Trie a b -> Trie a b -> Trie a b
union f (Trie b1 m1) (Trie b2 m2) =
    Trie b m
    where
      b = case (b1, b2) of
              (Nothing, b2) -> b2
              (b1, Nothing) -> b1
              (Just b1, Just b2) -> Just (f b1 b2)
      m = Map.unionWith (union f) m1 m2

ofList :: Ord a => [([a], b)] -> Trie a b
ofList l = foldr (\(k,v) t -> insert k v t) empty l


prop_CheckIfInsertedWordsAreMembers :: [([Char],Integer)] -> Bool
prop_CheckIfInsertedWordsAreMembers words =
    List.all (\(w,k) -> member w trie) words
    where trie = ofList words

prop_CheckIfInsertedWordsWithPrefixAreMembers :: [Char] -> [([Char],Integer)] -> Bool
prop_CheckIfInsertedWordsWithPrefixAreMembers pre words =
    List.all (\(w,k) -> member (pre ++ w) trie) words
    where trie = prefix pre (ofList words)

prop_CheckMembersForMergedTrie :: [([Char], Integer)] -> [([Char], Integer)] -> Bool
prop_CheckMembersForMergedTrie words1 words2 =
    List.all (\(w,k) -> member w trie) (words1 ++ words2)
    where
      trie = ofList words1 @@ ofList words2
      (@@) = union max
