module Ex08_Solution where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Function
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Transformers
import While3
                    
-- |---------------------|
-- | Matcher             |
-- |---------------------|

-- This is the direct implementation of Matcher. For the
-- transformer-based version see Ex08_Solution_MatcherTrans.hs
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

-- Combination of Reader and Error: a is the data that we match on and
-- b is the return type for a successfull match. We use the Error
-- Monad's ``abort'' ability to stop matching on a successful match...
data Matcher a b c = Matcher { rawMatcher :: a -> Either b c }

instance Monad (Matcher a b) where
  return x = Matcher $ const (Right x)
  m >>= f  = Matcher $ \x -> case rawMatcher m x of
                               Left y -> Left y
                               Right y -> rawMatcher (f y) x

-- A restricted version or rawMatcher. Usefull when we treat the
-- non-aborting case as a default return value.
runMatcher :: a -> Matcher a b b -> b
runMatcher x m = either id id $ rawMatcher m x

-- the ``atomic actions'' on matchers
matchAsk = Matcher $ Right . id 
found x = Matcher $ const (Left x)
under f m = Matcher $ \x -> rawMatcher m (f x)

-- the actions for needed for the example
matchWhen p r = matchAsk >>= \x -> when (p x) (found r)
matchExt e r = under (takeExt) $ matchWhen (e ==) r
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
-- | While Interpreter
-- |---------------------|

data Value = VInt Integer | VBool Bool
  deriving (Show, Eq)
type State = [(Id, Value)]
type Output = [Value]

-- The interpreter monad. ErrorT is used to abort execution when the
-- calculation do not make sense.
type MWhile = ErrorT String (StateT State (WriterT Output I))

-- Some usefull actions for the MWhile monad:
-- assignement (and allocation)
asgn :: Id -> Value -> MWhile ()
asgn i v = lift $ modify insertVar >> return ()
  where insertVar = insertBy (compare `on` fst) (i, v) . filter ((/= i) . fst) 
        modify f = get >>= put . f
-- checked access to a variable. If it is not defined an error is raised.
access i = maybe (mthrow $ "Variable " ++ i ++ " is not allocated")
                 return
           . lookup i =<< lift get
printResult i = lift $ lift $ tell [i]
-- The following actions perform ``type checking'' during
-- interpretation. If the argument does not match, the execution is
-- aborted.
checkInt (VInt i) = return i
checkInt v        = mthrow $ "Not an integer: " ++ show v
checkBool (VBool b) = return b
checkBool  v        = mthrow $ "Not a boolean: " ++ show v

-- main interpretation function. The first component of the result may
-- indicate an error.
runWhile :: State -> Program ->  (Either String (), State, Output)
runWhile initial (Program stmts) =
  mkResult $ exI $ exWriterT $ exStateT (exErrorT (evalStmts stmts)) initial
  where mkResult ((r, s), o) = (fmap (const ()) r, s, o)

-- the interpreter:
evalStmts :: [Stmt] -> MWhile ()
evalStmts = mapM_ evalStmt

evalStmt s@(While e stmts) = do
  b <- checkBool =<< evalExp e
  when (b) (evalStmts (stmts ++ [s]))
evalStmt (Asgn i e) = asgn i =<< evalExp e 
evalStmt (Print e) = printResult =<< evalExp e 

evalExp :: Exp -> MWhile Value
evalExp (Cmp op e1 e2) = do 
   b <- liftM2 (evalCOp op) (checkInt =<< evalExp e1)
                            (checkInt =<< evalExp e2)
   return $ VBool b
evalExp (If e e1 e2) = do
  x <- checkBool =<< evalExp e
  if x
  then evalExp e1
  else evalExp e2
evalExp (Not e) = do
  liftM (VBool . not) $ checkBool =<< evalExp e
evalExp (Num i) = return $ VInt i
evalExp (Op op e1 e2) = do
  i <- liftM2 (evalOp op) (checkInt =<< evalExp e1)
                          (checkInt =<< evalExp e2)
  return $ VInt i
evalExp (Var x) = access x

evalOp Plus   = (+) 
evalOp Minus  = (-)
evalOp Times  = (*)
evalOp Div    = div

evalCOp Eq   = (==)
evalCOp Neq  = (/=)
evalCOp Le   = (<=)
evalCOp Gt   = (>)

tests_while = testGroup "while interpreter"
  [ testProperty "while1" $ runWhile [] ex_while1
                           == (Right (), [("x", VInt 11), ("y", VInt 10000)], [VInt 10005])
  , testProperty "while2" $
      fmap (runWhile [("z", VInt 3)]) (parseProgram ex_while2_str)
      `shouldBe`
      ([("x", VInt 4), ("y", VInt (5^5)), ("z", VInt 3)], [VInt (5^5+5)])
  , testProperty "while_typeerror" $
      shouldBeError $ fmap (runWhile []) (parseProgram ex_while_typeerror_str)
  , testProperty "while_typeerror2" $
      shouldBeError $ fmap (runWhile []) (parseProgram ex_while_typeerror2_str)
  ] 
  where shouldBeError m = fromMaybe False $ fmap (isLeft . fst3) m
        isLeft (Left _) = True
        isLeft _        = False
        fst3 (x,_,_) = x
        shouldBe result (s, o) = result == Just (Right (), s , o )

ex_while_typeerror_str = 
  "\n\
  \ x:=0; y :=5;\n\
  \ while 10 do\n\
  \ y := (y * 5); x := (x + 1)\n\
  \ done;\n\
  \ y := if y > 10000 then 10000 else y fi\n\
  \ ;print (y + 5)\
  \ "

ex_while2_str = "\n\
  \  x:=0; y :=5;\n\
  \ while x <= z do\n\
  \ y := (y * 5); x := (x + 1)\n\
  \ done;\n\
  \ y := if y > 10000 then 10000 else y fi\n\
  \ ;print (y + 5)\
  \ "

ex_while_typeerror2_str = 
  "\n\
  \ x:=0; y :=5;\n\
  \ while x <= 10 do\n\
  \ y := (y * 5); x := (x + 1)\n\
  \ done;\n\
  \ y := if y > 10000 then 10000 else y fi\n\
  \ ;print (x + 5)\
  \ ;print (z + 5)\
  \ "
-- |---------------------|
-- | Tests
-- |---------------------|
runTests = defaultMain
  [ tests_matcher
  , tests_while
  ]         

