-- Das ist eine alternative Lösung zu Ex06.Aufgabe 4,
-- die die Instanzen für Monad, Applicative und Alternative von Parser verwendet.
module Ex06_Solution_Parser2 where

import Data.Char
import Data.List

import Parser2

import Test.Framework 
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

-- many ist schon in Control.Applicative für Alternative Instanzen vordefiniert
pmany :: Parser t r -> Parser t [r]
pmany = many 
-- trotzdem nochmal verschiedene Möglichkeiten für Implementierungen (alle äquivalent)
pmany' p = fmap (:) <*> p  <*> pmany' p
        <|> return []
pmany'' p = (:) <$> p <*> pmany'' p -- (<$>) ist infix für fmap
        <|> return []
pmany''' p = liftA2 (:) p (pmany''' p)

-- some ist auch schon vordefiniert
pmany1 :: Parser t r -> Parser t [r]
pmany1 = some

pIntList = spaces
         *> lit '['
         *>
         many (spaces
               *>
               pInt -- nur das wird zurückgegeben, Rest ignoriert
               <* spaces
               <* optional (lit ',')) -- optional ist auch aus Control.Applicative
         <* spaces
         <* lit ']'

pInt :: Parser Char Int
pInt = fmap read $ (slit '-' <|> pure "") `pApp` some (satisfy isDigit)

pTwice :: Eq t => Parser t [t] -> Parser t [t]
pTwice p = do
  w <- p  
  w' <- pstring w
  return $ w ++ w'

pPalindrom p = pPalindrom1 p <|> pure []
pPalindrom1 p = do
  w <- some p  -- parse ein Wort
  let rw = reverse w
  w' <- pstring rw <|> pstring (drop 1 rw) -- und dann die Umkehrung oder eines weniger als die Umkehrung
  return $ w ++ w'
         
-- helper
slit c = (:[]) <$> lit c
spaces = many (satisfy isSpace)
pApp p1 p2 = (++) <$> p1 <*> p2
pAnyLit = satisfy $ const True
pstring :: Eq t => [t] -> Parser t [t]
pstring = foldr (\c p -> (:) <$> lit c <*> p ) (pure [])


-- |------------------|
-- | Tests
-- |------------------|

runTests = defaultMain 
  [ testGroup "parsing"
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
          parse (pTwice (pstring xs <|> pstring ys))
                 (xs++xs)
          == Just (xs++xs :: [Int])
    , testProperty "twice pos 2" $
      \(NonEmpty xs) (NonEmpty ys) ->
         parse (pTwice (pstring xs <|> pstring ys))
               (ys++ys)
         == Just (ys++ys :: [Int])
    , testProperty "twice neg" $
      \(NonEmpty xs) (NonEmpty ys) ->
         xs /= ys ==> parse (pTwice (pstring xs <|> pstring ys))
                            (xs++ys :: [String])
                      == Nothing 
    ]
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
