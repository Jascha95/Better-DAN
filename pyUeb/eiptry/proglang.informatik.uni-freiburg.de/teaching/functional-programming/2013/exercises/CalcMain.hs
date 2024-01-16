module CalcMain where 

import Control.Monad (void)
import Data.List (foldl')
import Data.Char
  
import Graphics.UI.Threepenny hiding (map)
import qualified Graphics.UI.Threepenny.Core as UI
import Graphics.UI.Threepenny.Core hiding (children)
import Reactive.Threepenny

import Data.Enumerator (Enumerator, Enumeratee, (=$=), (=$), ($=), ($$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Functor.Identity       

-- | Run a stack calculator on a local server. To connect, point your
-- Browser to localhost:10000.
calcMain :: (String -> [Int] -> [Int]) -> String -> [Int] -> IO ()
calcMain step initProg init = do
    static <- return "."
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } $ setup step initProg init

setup :: (String -> [Int] -> [Int]) -> String -> [Int] -> Window -> IO ()
setup step initProg init w = void $ do
    w # set' title "FunPro Lib Test"
    (stackEl, setStack) <- stack
    progTextEl <- progText initProg
    exeEl <- exeButton
    resetEl <- resetButton
    getBody w #+ [column [ p # set text "Geben Sie ein Stack-Programm an (ein Befehl pro Zeile):"
                         , return progTextEl
                         , return exeEl
                         , return resetEl
                         , return stackEl
                         ]
                 ]
    -- Logic:
    progB <- stepper  initProg (valueChange progTextEl)
    let evalB :: Behavior (String -> [Int] -> [Int])
        evalB = pure $ flip $ eval step 
        evalStackB :: Behavior ([Int] -> [Int])
        evalStackB = evalB <*> progB

        evalOrResetB = withResetB evalStackB
        stepOrResetE = withResetE ([] <$ click resetEl) (click exeEl) 

    stackB <- accumB init (evalOrResetB <@> stepOrResetE)
    register (stepOrResetE) (\_ -> setStack =<< currentValue stackB)


------------------
-- Evaluation
------------------
eval :: (String -> [Int] -> [Int]) -> [Int] -> String -> [Int]
eval step st pStr = runIdentity $ E.run_ $
                      E.enumList 1 pStr $$ trimmedCommands step =$ EL.fold (#) st 
  where cmds = map step (filter (not . null) $ map trim $ lines pStr)
        trim = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace)

trimmedCommands :: Monad m => (String -> [Int] -> [Int]) -> Enumeratee Char ([Int] -> [Int]) m b
trimmedCommands step = EL.splitWhen (isSpace) =$= EL.filter (not . null) =$= EL.map step

------------------------
-- Event tools -------------
------------------------
data RunCommand a = Continue | Reset a
  deriving (Show, Eq, Ord)

withResetB :: Behavior (a -> b) -> Behavior (RunCommand b -> a -> b)
withResetB b = choose <$> b
  where choose f Continue = f
        choose f (Reset x) = const x

withResetE :: Event a -> Event b -> Event (RunCommand a)
withResetE resetE contE = unionWith const (Continue <$ contE) (Reset <$> resetE)

------------------------
-- The GUI -------------
------------------------
    
        
progText :: String -> IO Element
progText prog = textarea # set value prog

exeButton :: IO Element
exeButton = button # set text "Execute!"

resetButton :: IO Element
resetButton = button # set text "Reset stack"
           
executeButton :: Behavior String -> IO (Element, Event String)
executeButton prog = do
  el <- button # set text "Execute!"
  return (el, prog <@ click el)


                
stack :: IO (Element, Handler [Int])
stack = showDisplay "Stack"
-----------------
-- GUI utilities
-----------------
mkLabeled :: String -> Element -> IO Element
mkLabeled s e = row [p # set text s, return e]

showDisplay :: Show a => String -> IO (Element, Handler a)
showDisplay lab = do
  res <- p
  let handler x = res # set' text (show x)
  (,) <$> mkLabeled (lab ++ ": ") res <*> return handler
