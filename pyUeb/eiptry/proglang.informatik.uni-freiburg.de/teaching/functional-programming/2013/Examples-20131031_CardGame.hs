import Test.QuickCheck 

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq)

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

data Card = Card { suit :: Suit,  rank :: Rank }
  deriving (Show, Eq)

ex_card1 = Card Spades (Numeric 9)
ex_card2 = Card Diamonds (Numeric 9)
ex_card3 = Card Spades Ace

cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c
                        && rankBeats (rank givenCard)
                                     (rank c)

data Hand = Last Card | Next Card Hand
  deriving (Show, Eq)

ex_hand1 :: Hand
ex_hand1 = Next ex_card2 (Last ex_card3)

topCard :: Hand -> Card
topCard (Last c) = c
topCard (Next c _) = c
-- lastCard ..


test_choose_card1 = chooseCard ex_card1 ex_hand1 == ex_card3

-- returns a winning card, if possible, otherwise any card.
chooseCard :: Card -> Hand -> Card
chooseCard gc (Next c h) | cardBeats gc c = c                
                         | otherwise      = chooseCard gc h 
chooseCard gc (Last c) = c

prop_chooseCard c h = not (handBeats c h) ||  cardBeats c (chooseCard c (h))


handBeats :: Card -> Hand -> Bool
handBeats gc (Last c) = cardBeats gc c
handBeats gc (Next c h) = cardBeats gc c || handBeats gc h

-- returns a winning card, if possible, otherwise any card.
-- chooseCard :: Card -> Hand -> Card
-- chooseCard gc (c:cs) | cardBeats gc c = c
--                      | otherwise      = chooseCard gc cs
-- chooseCard [c] = c
-- chooseCard gc [] = error "empty hand!"



