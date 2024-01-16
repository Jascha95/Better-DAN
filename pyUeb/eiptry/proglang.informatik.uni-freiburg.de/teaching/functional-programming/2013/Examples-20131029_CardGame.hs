import Test.QuickCheck 

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show)

myFavoriteSuit :: Suit
myFavoriteSuit = Diamonds

data Color = Black | Red
  deriving (Show)

color :: Suit -> Color
color Spades = Black
color Hearts = Red
color Diamonds = Red
color Clubs = Black

              
data Rank = Ace | King | Queen | Jack | Numeric Int
  deriving (Show, Eq)

numeric :: Int -> Rank
numeric n | n > 0 && n <= 10 = Numeric n
          | otherwise = error ("There is no card with numeric rank " ++ show n)


rankBeats :: Rank -> Rank -> Bool
rankBeats Ace _   = False
rankBeats _   Ace = True
rankBeats King _  = False
rankBeats _   King = True
rankBeats Queen _ = False
rankBeats _ Queen= True
rankBeats  Jack _ = False
rankBeats  _ Jack = True
rankBeats (Numeric n) (Numeric n') = n' > n

test_rankBeats_ex1 = rankBeats (Numeric 8) Jack
-- transitive
prop_rankBeats_transitive (r1, r2, r3) =
  not ((rankBeats r1 r2) &&
       (rankBeats r2 r3)) ||
   rankBeats r1 r3
-- trichotomous 
prop_rankBeats_trichotomous (r1, r2) =
  xor3 (rankBeats r1 r2) 
       (rankBeats r2 r1) 
       (r1 == r2)

xor3 :: Bool -> Bool -> Bool -> Bool
xor3 True False False = True
xor3 False True False = True
xor3 False False True = True
xor3 _     _     _    = False

-- elements ::? [Rank] -> <generator>
allRanks :: [Rank]
allRanks = [Numeric n | n <- [2..10]] ++ [ Ace, King, Queen, Jack ]

allRankPairs = [(r1, r2) | r1 <- allRanks, r2 <- allRanks ]
allRankTriple = [(r1, r2, r3) | r1 <- allRanks, r2 <- allRanks, r3 <- allRanks ]

             



