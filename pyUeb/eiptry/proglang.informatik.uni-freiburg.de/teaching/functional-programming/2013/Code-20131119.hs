module Week5 where

import Test.QuickCheck
import Data.List

testgen = sample (arbitrary :: Gen Integer)

doTwice m = do
  x <- m
  y <- m
  return (x, y)

evenInteger :: Gen Integer
evenInteger = do
  n <- arbitrary
  return (2*n)

myoneof :: [Gen a] -> Gen a
myoneof gens = do
  let l = length gens
  i <- choose (0, l-1)
  gens !! i

data Suit = Spades | Hearts | Diamonds | Clubs 
  deriving (Show, Eq)
 
rSuit :: Gen Suit
rSuit = oneof [return Spades,
               return Hearts,
	       return Diamonds,
	       return Clubs]

myelements :: [a] -> Gen a
myelements = myoneof . map return

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show, Eq)

rRank = frequency [
          (4, elements [Jack, Queen, King, Ace]),
          (9, choose (2,10) >>= \n -> return $ Numeric n)]

rRank' = elements ([Jack, Queen, King, Ace] ++ [Numeric n | n <- [2..10]])

data Card = Card Rank Suit
  deriving (Show, Eq)

rCard = do
  rank <- rRank
  suit <- rSuit
  return $ Card rank suit

data Hand = Empty | Add Card Hand
  deriving (Show, Eq)

rHand =
  frequency [
    (1, return Empty),
    (4,  do card <- rCard
            hand <- rHand
            return $ Add card hand)]

instance Arbitrary Suit where
  arbitrary = rSuit

instance Arbitrary Rank where
  arbitrary = rRank

instance Arbitrary Card where
  arbitrary = rCard

instance Arbitrary Hand where
  arbitrary = rHand

-- datatype invariant for Rank
validRank :: Rank -> Bool
validRank (Numeric r) = 2 <= r && r <= 10
validRank _ = True

prop_validRank r = collect r (validRank r)

numCards :: Hand -> Integer
numCards Empty = 0
numCards (Add _ h) = 1 + numCards h

prop_validHand h = collect (numCards h) True

-- testing algorithms

ordered :: Ord a => [a] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

prop_insert :: Integer -> [Integer] -> Property
prop_insert x xs =
  collect (length xs) $
  ordered xs ==> ordered (insert x xs)

myorderedList :: Gen [Integer]
myorderedList = 
  frequency [(1, return []),
             (4, arbitrary >>= genOrdered)]

genOrdered s =
  frequency [(1, return [s]),
         (4, arbitrary >>= \i -> genOrdered (s + abs i) >>= \xs -> return (s : xs))]

newtype MyOrderedList = MyOrderedList [Integer]
  deriving Show

rOrderedList :: Gen MyOrderedList
rOrderedList =
  myorderedList >>= (return . MyOrderedList)

instance Arbitrary MyOrderedList where
  arbitrary = rOrderedList

prop_insert' x (MyOrderedList xs) =
  collect (length xs) $
  ordered xs ==> ordered (insert x xs)
