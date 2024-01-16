
data FileType = Haskell | Java | Tex | Binary | Other String
  deriving (Eq, Show)

fileType :: String -> FileType
fileType name = runMatcher name $ do
  matchExt "hs"    Haskell
  matchExt "java"  Java
  matchExt "tex"   Tex
  match    "a.out" Binary
  ext <- getExt
  return $ Other ext

tests_matcher =
  testGroup "matcher"
  [ testProperty "haskell" $ fileType "Hello.hs"   == Haskell 
  , testProperty "java"    $ fileType "Hello.java" == Java    
  , testProperty "tex"     $ fileType "Hello.tex"  == Tex     
  , testProperty "bin"     $ fileType "a.out"      == Binary  
  , testProperty "fail"    $ fileType "bla.blub"   == Other "blub"
  ]

