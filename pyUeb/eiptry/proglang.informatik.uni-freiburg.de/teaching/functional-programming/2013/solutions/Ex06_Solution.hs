module Ex06_Solution where

-- Utilities
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import Data.Maybe

-- Für Aufgabe 4
import Parser

-- Zum Testen
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- | Für Aufgabe 1 Importieren von ex05 für die Zahlenraten-Logik
import Ex05_Solution hiding (guess, main, runTests)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

-- |---------------------|
-- | Zahlenraten mit GUI |
-- |---------------------|
-- Gui setup
main = do
     startGUI defaultConfig
         { tpPort       = 10000
         , tpStatic     = Nothing
         } setup

-- initialization/start
setup :: Window -> IO ()
setup  w = do
  w # set' UI.title "Zahlenraten!"
  welcome <- UI.p # set UI.text "Denke an eine Zahl zwischen 0-100!"
  start <- UI.button # set UI.text "Start!"
  on UI.click start $ const (display w $ mkOutput w (interv 0 101))
  getBody w # set children [welcome, start]
  return () -- ^ dieses return ist nötig, damit der Rückgabewert
            -- stimmt (Alternative: ,,void'' aus Control.Monad)

-- Erstellt eine Liste von Köpfen [höher, tiefer, genau!] und versieht
-- sie mit den gegenenen Callbacks (CB, s.u.)
buttonRow :: CB -> [IO Element]
buttonRow cbs = mkBs
  where mkBs = zipWith setupButton 
               [ (choiceUp, "höher")
               , (choiceDown, "tiefer")
               , (choiceFound, "genau!")]
               (replicate 3 UI.button)
        setupButton (sel, t) mkB = do
          b <- mkB # set UI.text t
          on UI.click b (const $ sel cbs)
          return b

-- zeige die Reaktion (auf eine Benutzereingabe, s.u.) im Fenster an
display :: Window -> Reaction -> IO ()
display w (cbs, out) =
  do body <- column [ UI.p # set UI.text out
                    , row $ buttonRow cbs]

     getBody w # set UI.children [body]
     return ()
  
-- A CB (CallBack) is a Combination of 3 actions that should execute
-- when the respective buttons `up', `down', or `found' are pressed
data CB = CB { choiceUp :: IO (), choiceDown :: IO (), choiceFound :: IO () }
trivialCB = CB (return ()) (return ()) (return ())

-- Die Reaktion auf eine Benutzereingabe ist ein neuer Satz Callbacks
-- und eine Nachricht, die dem Benutzer angezeigt wird.
type Reaction = (CB, Message)
type Message = String


-- Berechne eine angemessene Reaktion auf das gegebene neue
-- Rate-Interval.
mkOutput :: Window -> Interval Int -> Reaction
mkOutput w i = ask $ bisect i -- ^ Versuche das Intervall zu teilen
                              -- und die nächste Frage zu stellen
  where -- Wenn sich das intervall weiter teilen lässt, dann stelle
        -- die nächste Frage Die Callbacks sind erhalten dann genau
        -- die korrekte Hälfte für die nächste Antwort.
        ask (Just (l, x, u)) = (CB (guessCB u) (guessCB l) (win x), guess x)
        -- Wenn sich das Interval nicht teilen lässt, hat der Benutzer
        -- auf einen Knopf gedrückt, der keine Wahlmöglichkeiten mehr
        -- enthält.
        ask Nothing          = (trivialCB, accuse) 

        -- Callback für größer/kleiner Knöpfe: mit einem verkleinerten Intervall fragen
        guessCB i' = display w $ mkOutput w i'
        -- Callback für ,,genau'': geben an und schalte alle Knöpfe ab
        win x      = display w $ (trivialCB, gloat x)

        -- Mögliche Nachrichten
        accuse = "Ich spiele nicht mit Lügnern!"
        guess x = "Ist es die `" ++ show x ++ "'?"
        gloat x = "Hah! Es ist die `" ++ show x ++ "'! Ich habe gewonnen!"

-- |------------------|
-- | Striktheit       |
-- |------------------|

-- foldr f z xs: strikt in xs
-- foldl f z xs: strikt in xs
-- take n xs : strikt in n
-- zipWith f xs ys : strikt in ys
-- unfoldrf z : strikt in f

  
tests_strict =
  testGroup "strictness"
  [ testProperty "foldr f" $ foldr undefined 0 [] == 0
  , testProperty "foldr z" $ foldr (\ x _ -> x)  undefined [1,2,3] == 1 
  , testProperty "foldl f" $ foldl undefined 0 [] == 0
  , testProperty "foldl z" $ foldl (\ _ x -> x)  undefined [1,2,3] == 3
  , testProperty "take xs" $ take 0 undefined == ([] :: [Int])
  , testProperty "zipWith f ys" $ zipWith undefined [] undefined == ([] :: [Int])
  , testProperty "unfoldr z" $ unfoldr (const Nothing) undefined == ([] :: [Int])
  ]

-- |---------------------|
-- | Newton
-- |---------------------|
sqrtNewton :: Double -> (Double -> [Double] -> Double) -> Double -> Double -> Double
sqrtNewton eps approx a r = approx eps ais
  where ais = iterate (\x -> (x + r / x) / 2) a
        
untilAbs eps (x:y:xs)  | abs (x - y) < eps = y
                       | otherwise         = untilAbs eps (y:xs)
untilAbs _ _  = error "List too short for approximation"

untilRel eps (x:y:xs)  | abs (1 - x / y) < eps = y 
                       | otherwise             = untilRel eps (y:xs)
untilRel _ _ = error "List too short for approximation"

ex_eps = 1e-5
nsqrt = sqrtNewton ex_eps untilAbs 5 
nsqrt_rel = sqrtNewton ex_eps untilRel 5
   
tests_newton =
  testGroup "newton" 
  [ testProperty "until abs" $ \(Positive x) ->  nsqrt x `goodEnough` sqrt x
  , testProperty "until rel" $ \(Positive x) ->  nsqrt_rel x `goodEnough` sqrt x
 ]
  where goodEnough x y = abs (1 - x / y) < 0.02

-- |------------------|
-- | Parsing
-- |------------------|

-- The order is actually important!
pmany :: Parser t r -> Parser t [r]
pmany p = (pmap (:) p `pseq` pmany p) `palt` succeed [] 

pmany1 :: Parser t r -> Parser t [r]
pmany1 p = pmap (:) p `pseq` pmany p

pIntList :: Parser Char [Int]
pIntList = right spaces
           $ right (lit '[')
           $ left
             (pmany ((right spaces
                     $ left pInt
                       (left spaces (lit ',' `palt` succeed '_'))) :: Parser Char Int))
           $ left spaces (lit ']') -- Parser.empty -- (pmany pAnyLit)
pInt :: Parser Char Int
pInt = pmap read $ (slit '-' `palt` succeed "") `pApp` pmany1 (satisfy isDigit)

pPalindrom :: Eq t => Parser t t -> Parser t [t]
pPalindrom p = pali_none `palt` pali_one `palt` pali_gen
  where pali_none = succeed []
        pali_one = pmap (:[]) p
        pali_gen ts = [ (r:rs, ts'')
                      | (r, ts') <- p ts
                      , (rs, ts'') <- (pPalindrom p `pApp` slit r) ts'
                      ]

pPaliAB = pPalindrom (lit 'a' `palt` lit 'b')

pTwice p ts = [ (r ++ r', rest') | (r, rest) <- p ts, (r', rest') <- pstring r rest]

parse :: Parser t r -> [t] -> Maybe r
parse p ts = case dropWhile (not . null . snd) (p ts) of
  []    -> Nothing
  ((res,_):_) -> Just res


--------------
-- helpers
--------------
-- parse a char as a string
slit c = pmap (:[]) $ lit c
-- any literal
pAnyLit = satisfy $ const True
-- append to string parsers
pApp p1 p2 = pmap (++) p1 `pseq` p2
-- throw away the second result
left :: Parser t a -> Parser t b -> Parser t a
left p1 p2 = pmap const p1 `pseq` p2
-- throw away the first result
right :: Parser t a -> Parser t b -> Parser t b
right p1 p2 = pmap (flip const) p1 `pseq` p2
-- match 0 or more spaces
spaces = pmany (satisfy isSpace)
-- parse exactly the given string
pstring :: Eq t => [t] -> Parser t [t]
pstring xs = foldr (\c s -> pmap (:) (lit c) `pseq` s) (succeed []) xs

tests_parsers =
  testGroup "parsing"
  [ testProperty "many" $
     \(RC (c, s)) -> parse (pmany $ lit c) s == Just s
  ,testProperty "many fail" $
     \(EAL (c, s)) -> parse (pmany $ lit c) s == Nothing
  , testProperty "many1" $
     \(RC (c, s)) -> not (null s) ==> parse (pmany1 $ lit c) s == Just s
  , testProperty "many1fail" $
    \c -> parse (pmany1 $ lit c) ([]:: [String]) == Nothing
  , testProperty "intlist" $
    \is -> parse pIntList (show is) == Just is
  , testProperty "intlist 2" $
    forAll (listOf $ choose (0, 9)) $
    \is -> parse pIntList (intersperse ' ' $ show is) == Just is
  , testProperty "intlist fail 1" $ forAll (resize 7 arbitrary) $
    \is -> (parse pIntList (show (is :: [Int]) ++ "  ") == Nothing)
  , testProperty "palindrom (even)" $
    \xs -> parse (pPalindrom pAnyLit) (xs++ reverse xs) == Just (xs++ reverse xs :: [Int])
  , testProperty "palindrom (odd)" $
    \x xs -> parse (pPalindrom pAnyLit) (xs++ x:reverse xs) == Just (xs++ x:reverse xs :: [Int])
  , testProperty "twice pos 1" $
    \(NonEmpty xs) (NonEmpty ys) ->
        parse (pTwice (pstring xs `palt` pstring ys))
               (xs++xs)
        == Just (xs++xs :: [Int])
  , testProperty "twice pos 2" $
    \(NonEmpty xs) (NonEmpty ys) ->
       parse (pTwice (pstring xs `palt` pstring ys))
             (ys++ys)
       == Just (ys++ys :: [Int])
  , testProperty "twice neg" $
    \(NonEmpty xs) (NonEmpty ys) ->
       xs /= ys ==> parse (pTwice (pstring xs `palt` pstring ys))
                          (xs++ys :: [Int])
                    == Nothing
  ]
  
newtype RepeatedChar = RC (Char, String)
  deriving (Show)

instance Arbitrary RepeatedChar where
  arbitrary = do
    c <- arbitrary
    cs <- listOf $ return c
    return $ RC (c, cs)


newtype ElementAndList = EAL (Char, String)
  deriving (Show)
instance Arbitrary ElementAndList where
  arbitrary = do
    s <- arbitrary `suchThat` notMany
    c <- elements s
    return $ EAL (c, s)
    where notMany [] = False
          notMany (x:xs) = any (/=x) xs

-- |------------------|
-- | Tests
-- |------------------|
runTests = defaultMain
  [ tests_strict
  , tests_newton
  , tests_parsers
  ]

