module Ex08_Solution_MatcherTrans where
-- This is the matcher version with transformers

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Transformers
import While2
                    
-- |---------------------|
-- | Matcher  (with transformers)            |
-- |---------------------|

data FileType = Haskell | Java | Tex | Binary | Other String
  deriving (Eq, Show)

fileType :: String -> FileType
fileType name = runMatcher name $ do
  matchExt "hs" Haskell
  matchExt "java" Java
  matchExt "tex" Tex
  match "a.out"  Binary
  ext <- getExt
  return $ Other ext

type Matcher a b = ReaderT a (ErrorT b I)

runMatcher :: a -> Matcher a b b -> b
runMatcher x m = either id id $ exI $ exErrorT $ exReaderT m x


matchAsk :: Matcher a b a
matchAsk = ask
found x = lift $ mthrow x
under f m = local f m

matchWhen p r = matchAsk >>= \x -> when (p x) (found r)
matchExt e r = under (takeExt) $ matchWhen (e `isSuffixOf`) r
matchExt' e r = matchWhen (e `isSuffixOf`) r
match s = matchWhen (==s)
getExt = under takeExt matchAsk
takeExt s = reverse $ takeWhile (/= '.') (reverse s)

tests_matcher =
  testGroup "matcher"
  [ testProperty "haskell" $ fileType "Hello.hs" == Haskell
  , testProperty "java" $ fileType "Hello.java" == Java
  , testProperty "tex" $ fileType "Hello.tex" == Tex
  , testProperty "bin" $ fileType "a.out" == Binary
  , testProperty "fail" $ fileType "bla.blub" == Other "blub"
  ]

-- |---------------------|
-- | Tests
-- |---------------------|
runTests = defaultMain
  [ tests_matcher
  ]         
