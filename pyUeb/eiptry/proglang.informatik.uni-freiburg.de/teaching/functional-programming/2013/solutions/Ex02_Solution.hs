module Ex02_Solution where
import Data.List

import Tracks

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
  
-- |---------------------------------------------|
-- | Fib
-- |---------------------------------------------|
fib :: Integer -> Integer
fib n | n < 0 = error "Fib is not defined for n < 0"
      | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n - 1) + fib (n - 2)

fastFib :: Integer -> Integer
fastFib n | n < 0 = error "Fib is not defined for n < 0"
          | n == 0 = 0
          | n == 1 = 1
          | otherwise = fastFibRec n 2 (1, 0) 
fastFibRec n i (f1, f2) | n == i = f1 + f2
                        | otherwise = fastFibRec n (i+1) (f1 + f2, f1)

prop_fastFib_is_fib n = n >= 0 ==> fastFib n == fib n
test_firstFibs = (map fib [0..12]) == [0,1,1,2,3,5,8,13,21,34,55,89,144]
test_bigFib = (fastFib 20000 == fastFib 20000)
fibTests = testGroup "Fibunacci Tests"
           [testProperty "first 13 fibs" test_firstFibs
           , testProperty "fastFib is fib" (forAll (resize 22 arbitrary) prop_fastFib_is_fib)
           , testProperty "big fib" test_bigFib
           ]

-- |---------------------------------------------|
-- | undup
-- |---------------------------------------------|
undup (x:xs) = x : undup ([y | y <- xs, y /= x])
undup [] = []


tests_undup = testGroup "undup"
  [
    testProperty "ex1" $ undup [1,1,3, 4, 3] == [1, 3, 4]
  , testProperty "ex2" $ undup [4, 5, 6, 7 ] == [4, 5, 6, 7 ]
  , testProperty "only once" $ prop_undup_only_once
  , testProperty "removes product" $ forAll (resize 30 arbitrary) prop_undupRemove_product
  , testProperty "undup unique" $ prop_undupUnique
  ]

prop_undup_only_once :: [Int] -> Bool
prop_undup_only_once xs = undup xs == undup (undup xs)
prop_undupRemove_product :: [Int] -> Bool
prop_undupRemove_product xs = undup (concat [[x, y] | x <- undup xs, y <- undup xs]) == undup xs
isUniqueIn x xs = length [y | y <- xs, y == x] == 1
prop_undupUnique :: [Int] -> Bool
prop_undupUnique xs = and [ isUniqueIn x (undup xs) | x <- xs ]



showBoard b = "[" ++ intercalate "\n, " (map show b) ++ "]"
showBoards bs = "[" ++ intercalate "\n\n, " (map showBoard bs) ++ "]"

-- |---------------------------------------------|
-- | Kleinster Faktor
-- |---------------------------------------------|
smallestFactor :: Integer -> Integer
smallestFactor n  = nextFactor 2 n
               
smallestFactor' n = head [ k | k <- [2..], k `divides` n]

prop_smallestFactor_is_smallestFactor' n = smallestFactor n == smallestFactor' n
               
-- | 'divides k n' returns true if k divides n
divides :: Integer -> Integer -> Bool
divides k n = n `mod` k == 0

nextFactor :: Integer -> Integer -> Integer
nextFactor 0 n = nextFactor 2 n
nextFactor 1 n = nextFactor 2 n           
nextFactor k n | k >= 2 && k `divides` n = k
nextFactor k n | k >= 2 = nextFactor (k+1) n 

-- |---------------------------------------------|
-- | Musikbibliothek
-- |---------------------------------------------|


type User = String

data Tune = Tune { name :: String, artist :: String, duration :: Int }
  deriving (Show, Eq)

data Album = Album { aName :: String, aTunes :: [Tune] }
  deriving (Show, Eq)

data LikeVal = Bad | Good
  deriving (Show, Eq)

data Like = Like { user :: String, tune :: Tune, like :: LikeVal }
  deriving (Show, Eq)

data Bib = Bib { tunes :: [Tune], likes :: [Like], albums :: [Album] }
  deriving (Show, Eq)

---------------------------
-- Build a library using Tracks.hs
---------------------------
bib :: Bib
bib = Bib bibTunes bibLikes bibAlbums 

-- Helper, to translate Tuples to Tunes
mkTune (t, a, l) = Tune t a l

bibTunes = map mkTune $ noAlbum ++ electronic ++ shifted ++ collective


bibLikes = alfonsGood ++ alfonsBad ++ bettyGood ++ bettyBad
  where alfonsGood    = map (mkLike "Alfons" Good) [10,4,9,2,8]
        alfonsBad     = map (mkLike "Alfons" Bad) [12,3,13,11] 
        bettyGood     = map (mkLike "Betty" Good) [13,0,11]
        bettyBad      = map (mkLike "Betty" Bad) [5,9]
        mkLike n l k  = Like n (bibTunes !! k) l

bibAlbums = [ Album "Electronic Warfare 2.0" $ map mkTune electronic
            , Album "Shifted Phases" $ map mkTune shifted 
            , Album "The Collective" $ map mkTune collective
            ]
              
              
----------------------------
-- Operations
----------------------------
addAlbum :: Tune -> String -> Bib -> Bib
addAlbum t a b = setAlbums b updAlbums
  where updAlbums = updated : filter (/= original) (albums b) 
        updated :: Album
        updated = addToAlbum original t 

        original :: Album
        -- Note the use of the dollar. It is just function application
        -- that binds very weakly and associates to the right
        original = head $ [ a' | a' <- albums bib, aName a' == a ]
                          ++ [Album a []] -- in case it's a new album
        -- alternative without dollar
        original' = head ([ a' | a' <- albums bib, aName a' == a ]
                          ++ [Album a []]) -- in case it's a new album

        addToAlbum (Album n ts) t = Album n (t:ts)

        -- alternative using record update syntax
        addToAlbum' a@(Album _ ts) t = a { aTunes = t : ts }
        -- alternative using record update syntax and record pattern syntax
        addToAlbum'' a@(Album { aTunes = ts }) t = a { aTunes = t : ts }

        setAlbums (Bib ts ls _) as = Bib ts ls as
        -- alternative using record syntax
        setAlbums' b as = b { albums = as }

-- Note: partial application
rateTrack :: User -> Tune -> LikeVal -> Bib -> Bib
rateTrack u t l bib = bib { likes = update (likes bib) }
  where newLike = Like u t l
        update ls =  newLike : filter (doesNotMatch newLike) ls
        -- without partially applied functions
        update' ls =  newLike : [l | l <- ls, doesNotMatch newLike l ]
        doesNotMatch (Like u t _) (Like u' t' _) = not (u == u' && t == t')
                      
----------------------------
-- Queries
----------------------------
albumLengths :: Bib -> [(String, Int)]
albumLengths bib = map mkResult $ albums bib
  where mkResult a = (aName a, sum (map duration $ aTunes a))

goodAlbums :: User -> Bib -> [Album]
goodAlbums u bib = [a | a <- albums bib, majorityGood a]
  where goodTunes = [t | Like u' t lv <- likes bib, u == u' && lv == Good]
        -- Alternative: use pattern selection in list comprehensions:
        goodTunes' = [t | Like u' t Good <- likes bib, u == u']
        isGood t = t `elem` goodTunes
        majorityGood a = length (filter isGood $ aTunes a) > length (aTunes a) `div` 2

-- |------------------------|
-- | Tic Tac Toe
-- |------------------------|

--------------------------
-- Datatypes
data Player = Cross | Circle 
  deriving (Show, Eq)

-- Alternative: Maybe Player
data Field = Empty | Set Player
           deriving (Show, Eq)

-- For convenience
cross :: Field
cross = Set Cross

circ :: Field
circ = Set Circle

type Board = [[Field]]

-- utility functions on boards
flatBoard :: Board -> [Field]
flatBoard = concat

unflatBoard :: [Field] -> Board
unflatBoard b = [take 3 b, take 3 (drop 3 b), drop 6 b]

count :: Field -> Board -> Int
count f g = length $ filter (== f) $ flatBoard g

-- The possible states of a board
data GameState = InProgress | Won Player | Draw | Invalid
  deriving (Show, Eq)


--------------------------
-- some boards
initBoard :: Board
initBoard = [ replicate 3 row | row <- replicate 3 Empty ]
initBoard' = [ [Empty, Empty, Empty ]
             , [Empty, Empty, Empty ]
             , [Empty, Empty, Empty ]]

ex_board1 = [[circ, circ, circ],
             [cross, circ, cross],
             [circ, cross, cross]]

ex_board2 = [[circ, cross, circ],
             [cross, circ, cross],
             [cross, circ, cross]]

ex_board3 = [[circ, cross, circ],
             [cross, Empty, cross],
             [cross, circ, cross]]



--------------------------
-- Calculate the state of a board
calculateState :: Board -> GameState
calculateState g | not (validTurns g && validBoard g) = Invalid
                 | unfinished g = InProgress
                 | any unfinished (clearOne g) = evalGame g
                 | otherwise = Invalid


-------
-- Determine who won a valid and finished game
evalGame g = head $ [Won p |  winRow <- winnings g, Set p <- winRow]
                     ++ [Draw] 


-- enumerate all rows, columns and diagonals that are fully populated by some player
winnings g = filter (allCrossOrCirc) (enumWinningFields g)
  where isCross f = f == cross
        isCirc f = f == circ
        allCrossOrCirc fs = all isCross fs || all isCirc fs

enumWinningFields :: Board -> [[Field]]
enumWinningFields b = b ++ transpose b
                      ++ [[ b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2 ]]
                      ++ [[ b !! 2 !! 0, b !! 1 !! 1, b !! 0 !! 2 ]]

-- The board was produced by alternating terms
validTurns :: Board -> Bool
validTurns b = abs (abs (crossCount) - abs (circCount)) <= 1 
  where crossCount = count cross b
        circCount = count circ b


validBoard :: Board -> Bool
validBoard b = isThreeList b && all isThreeList b
  where isThreeList l = length l == 3

unfinished :: Board -> Bool
unfinished g = null (winnings g)
               && count Empty g /= 0

clearOne :: Board -> [Board]
clearOne g = map unflatBoard [ set i Empty fg | i <- [0..length fg - 1] ]
  where set n x xs = take(n) xs ++ (x:drop (n+1) xs)
        fg = flatBoard g

            
extensions b | unfinished b = filter (`extendsValidly` b) enum_boards
             | otherwise = []

b' `extendsValidly` b = b' `extends` b && isFinished b'
isFinished b = st == Draw || st == Won Cross || st == Won Circle
  where st = calculateState b 

extends :: Board -> Board -> Bool
b1 `extends` b2 = and [  f1 `canOverwrite` f2 | (f1, f2) <- alignedBoards ]
  where _ `canOverwrite` Empty  = True
        f `canOverwrite` f'     = f == f' 

        alignedBoards            = zip (flatBoard b1) (flatBoard b2)
        
-- Alternative, without zip
extends' b1 b2 = and [ (fb1 !! k) `canOverwrite` (fb1 !! k) | k <- [0..length fb1 - 1]]
  where fb1 = flatBoard b1
        fb2 = flatBoard b2

        _ `canOverwrite` Empty  = True
        f `canOverwrite` f'     = f == f' 

        
enum_boards = [ [r1, r2, r3] | r1 <- enum_rows,
                               r2 <- enum_rows,
                               r3 <- enum_rows ]
enum_rows = [[f1, f2, f3] | f1 <- enum_fields,
                            f2 <- enum_fields,
                            f3 <- enum_fields]
enum_fields = [Empty, cross, circ]

-- Some aux board predicates
valid b = Invalid /= calculateState b
won p b = Won p == calculateState b
draw b = Draw == calculateState b
invalidWins b = not (valid b) && validBoard b && validTurns b


ttt_tests =
  [ testGroup "validity"
    [ testGroup "invalid boards (size)"
      [ testProperty "ex1" $ not $ valid []
      , testProperty "ex2" $ not $ valid
        [[Empty, circ, cross],
         [Empty, circ, cross],
         [Empty, Empty]]
      , testProperty "ex3" $ not $ valid
        [[Empty, circ, cross],
         [Empty, circ, cross],
         [Empty, Empty, Empty],
          [Empty, Empty, Empty]]
       , testProperty "ex4" $ not $ valid
         [[Empty, circ],
          [Empty, circ, cross],
          [Empty, Empty, Empty]]
       , testProperty "ex4" $ not $ valid
         [[Empty, circ, Empty, Empty],
          [Empty, circ, cross],
          [Empty, Empty, Empty]]
       ]
    , testGroup "invalid boards (turns)"
      [ testProperty "ex1" $ not $ valid
        [[Empty, circ, circ],
         [Empty, circ, circ],
         [Empty, Empty, Empty]]
      , testProperty "ex2" $ not $ valid
        [[circ, circ, circ],
         [circ, circ, circ],
         [circ, circ, circ]]
      , testProperty "ex3" $ not $ valid
        [[cross, cross, circ],
         [circ, circ, cross],
         [Empty, cross, cross]]
      ]
    , testGroup "valid boards"
      [ testProperty "init" $ valid initBoard 
      , testProperty "ex1" $ valid [[Empty, circ, cross],
                                    [Empty, circ, cross],
                                    [Empty, Empty, Empty]]
      , testProperty "ex2" $ valid [[cross, circ, cross],
                                    [Empty, circ, cross],
                                    [Empty, Empty, Empty]]
      , testProperty "ex3" $ valid [[cross, circ, cross],
                                    [Empty, circ, cross],
                                    [circ, Empty, circ]]
      , testProperty "ex4" $ valid [[circ, circ, circ],
                                    [cross, circ, cross],
                                    [circ, cross, cross]]
      ,testProperty "ex5" $ valid
        [[circ, circ, circ],
         [cross, Empty, cross],
         [Empty, cross, Empty]]
      , testProperty "ex6" $ valid
        [[circ, circ, circ],
         [circ, cross, cross],
         [circ, cross, cross]]
      ]

    , testGroup "invalid boards (wins)"
      [ testProperty "ex2" $ invalidWins
        [[circ, circ, circ],
         [cross, cross, cross],
         [Empty, cross, Empty]]
      ]
    ]
  , testGroup "wins"
    [ testGroup "wins"
      [ testProperty "ex1" $ won Cross
        [[circ, circ, Empty],
         [cross, cross, cross],
         [Empty, cross, circ]]
      , testProperty "ex2" $ won Circle
        [[circ, circ, cross],
         [cross, circ, cross],
         [Empty, cross, circ]]
      ]
      , testGroup "draws"
        [ testProperty "ex1" $ draw
          [[circ, cross, circ],
           [cross, circ, cross],
           [cross, circ, cross]]
        ]
      ]
    , testGroup "Extensions"
      [ testGroup "no extensions"
        [ testProperty "ex1" $ not $ [[cross, cross]] `extends` [[Empty, circ] ]
        , testProperty "ex2" $ not $ [[Empty, Empty]] `extends` [[Empty, circ]]
        ]
      , testGroup "extensions"
        [ testProperty "ex1" $ [[cross, cross], [Empty, cross]] `extends`
          [[Empty, cross], [Empty, Empty]]
        , testProperty "ex2" $ [[cross, cross], [cross, cross]] `extends`
          [[cross, cross], [cross, cross]]
        ]
      ]
  ]


