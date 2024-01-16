module Ex05_Solution where 

import Control.Monad
import Data.List


import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Ex02_Solution


-- |-------------------|
-- | Zahlenraten       |
-- |-------------------|

-- Die Ausgedachte Zahl wird sich in immer kleiner werdenden
-- Intervallen befinden. Intervall sind 
data Interval a = Interval { lowerBound :: a -- ^ inklusiv
                           , upperBound :: a -- ^ exclusiv
                           }
  deriving (Eq)

instance Show a => Show (Interval a) where
  show (Interval l u) = "[" ++ show l ++"-" ++ show u ++ ")"

-- ,,Smart''-Konstruktor, der ungültige Intervalle abfängt
interv x y | x < y = Interval x y
interv x y | otherwise = Interval x x

-- Anzahl der Elemente in einem Intervall
iabs :: Num a => Interval a -> a
iabs i = upperBound i - lowerBound i

-- Standardfunktionen für Intervalle
iEmpty (Interval x y) = x == y
iElem :: Ord a => a -> Interval a -> Bool
iElem x (Interval l u) = l <= x && x < u

-- Halbieren eines Intervalls
bisect :: (Ord a, Integral a) => Interval a -> Maybe (Interval a, a, Interval a)
bisect i |  iEmpty i   = Nothing
         | otherwise = Just (interv low mid, mid, interv (mid + 1) high) 
  where low  = lowerBound i
        high = upperBound i
        mid = low + iabs i `div` 2


main :: IO ()
main = do
  putStrLn "Wähle eine Zahl zwischen 1 und 100. (Weiter mit Return)"
  getLine
  -- Aufrung des Spiels mit initialem Intervall
  guess $ interv 1 101

-- Mögliche Antworten
data Choice = Up | Down | Exact
  deriving (Eq, Show)

guess i = do
  -- Rate den Mittleren Wert
  --  falls es ihn nicht gibt, hat der Spieler gelogen
  --  ansonsten fragen wir ihn
  maybe accuse ask $ bisect i

  where accuse = putStrLn "Ich spiele nicht mit Lügnern!"

        ask cand@(_, x, _) = do
          react cand =<< getChoice ("Ist es die `" ++ show x ++ "'?")

        -- ... Reaktion auf die Antwort
        react (_, _, h) Up = guess h
        react (l, _, _) Down = guess l
        react _        Exact = putStrLn "Hah! Gewonnen!"

-- Fragt so lange, bis die Frage mit einer gültigigen Wahl beantwortet
-- ist.
getChoice :: String -> IO Choice
getChoice q = do
  putStr q
  l <- getLine
  case l of
    "höher" -> return Up
    "tiefer" -> return Down
    "genau" -> return Exact
    _       -> putStrLn "Fehler: ungültige Eingabe!" >> getChoice q
  
          
          
-- |---------------------|
-- | Listenpaare         |
-- |---------------------|
listOfLength :: Integer -> Gen a -> Gen [a]
listOfLength n = sequence . genericReplicate n 

pairsOfLists :: Integer -> Gen a -> Gen ([a], [a])
pairsOfLists n g = do
  l1 <- listOfLength n g
  l2 <- listOfLength n g
  return (l1, l2)

gen_somePairsOfLists = do
  n <- arbitrary
  pairsOfLists n (arbitrary :: Gen Integer)


-- Die mathematische Definition von `f ist invers zu g' wäre in etwa
-- (f . g ==== id). Das funktioniert so nicht für `zip' und `unzip',
-- da `zip' 2 Parameter hat. Die Prelude Funktion `uncurry' verwandelt
-- eine zweistellige Funktion in eine einstellige, die ein 2-Tupel mit
-- den beiden Argumenten erwartet.
prop_unzip_zip :: [(Integer, Integer)] -> Bool
prop_unzip_zip ps = (uncurry zip . unzip) ps == ps
prop_zip_unzip :: ([Integer], [Integer]) -> Bool
prop_zip_unzip p_xs = (unzip . uncurry zip) p_xs == p_xs


tests_listpairs =
  testGroup "Listenpaare"
  [ testProperty "pairs of lists" $
    forAll (gen_somePairsOfLists) $ \(xs, ys) -> length xs == length ys
  , testProperty "zip . unzip" $ prop_unzip_zip
  , testProperty "unzip . zip" $ forAll gen_somePairsOfLists prop_zip_unzip
  ] 

-- |---------------------|
-- | TicTacToe           |
-- |---------------------|

instance Arbitrary Field where
  arbitrary = do
    elements $ Empty : [Set p | p <- [Circle, Cross]]

newtype TestBoard = TB Board
  deriving Show


gen_permute [] = return []
gen_permute xs = do
  n <- choose (0, length xs - 1)
  let x = xs !! n
  rest <- gen_permute $ deletedAt n
  return $ x : rest
  where deletedAt n = take n xs ++ drop (n+1) xs

instance Arbitrary TestBoard where
  arbitrary = do
    nx <- choose (0, 9 `div` 2)
    ny <- choose (0, nx + 1) 
    let no = 9 - nx - ny
    xs <- listOfLength nx $ return $ Set Cross
    ys <- listOfLength ny $ return $ Set Circle
    os <- listOfLength no $ return $ Empty
    b <- gen_permute $ xs ++ ys ++ os
    return $ TB $ unflatBoard b

-- Falsch:
gen_board = do
  b <- arbitrary :: Gen Board
  (arbitrary :: Gen Board) `suchThat` valid 

collectBoards = quickCheckWith stdArgs{ maxSuccess = 1000 } $
                \(TB b) -> collect (valid b) True


main' = putStrLn "Hello" >> putStrLn "World"

-- |---------------------|
-- | Tests
-- |---------------------|

runTests =
  defaultMain
  [ tests_listpairs
  ]
