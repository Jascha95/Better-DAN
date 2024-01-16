{-# LANGUAGE GADTs #-}
module Robot
       ( Gold ()
       , Result
       , Robot ()
       , idle
       , mine
       , scan
       , go
       , (>+>)
       , (<->)
       , runRobot
       , timeRobot
       , robprog, robprog', somemining
       ) where

import Prelude hiding (putStr, putStrLn)
import qualified Prelude
import Data.Monoid
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Random
import System.IO hiding (putStr, putStrLn)
import Data.Time.Clock

data RState = RState { rsPosition :: Int
                     , rsMessage :: String
                     , rsOldMessage :: String }

-- changing the position also deletes the message
withPos :: (Int -> Int) -> RState -> RState
withPos f rs@(RState { rsPosition = p }) = rs{ rsPosition = f p } -- ,  rsMessage = ""} 
withMessage :: (String -> String) -> RState -> RState
withMessage f rs@(RState { rsMessage = m}) = rs{ rsMessage = f m }
        
type RWorld = StateT RState IO

putStr :: String -> RWorld ()
putStr s = modify (withMessage (++s))

incPos :: Int -> RWorld ()
incPos i = modify (withPos (+i))
getPos :: RWorld Int
getPos = gets rsPosition

data Robot a where
     Robot :: (Show a) => (RWorld a) -> Robot a

-- Actions in the Robot World
msg m = (putStr $ " : " ++ m) >> drawState >> wait 4

drawState = do
  pos <- getPos
  modify $ withMessage (trace pos ++)
  update
  where update = do
          rs@RState {rsMessage = m, rsOldMessage = old} <- get
          liftIO $ do
            replicateM_ (length old) $ Prelude.putChar '\BS' 
            replicateM_ (length old) $ Prelude.putChar ' ' 
            Prelude.putChar '\CR'
            Prelude.putStr m
          put $ rs{ rsMessage = "", rsOldMessage = m } -- new becomes old

        trace i = replicate i '.' ++ "o"

wait i = liftIO $ threadDelay (i * 1000000 `div` 4)  

move dist = forM_ [0..(abs dist - 1)] $ \_ -> wait 1 >> incPos (signum dist) >> drawState


runRobot c = runStateT (goHome c) $ RState 0 "" ""
unRobot (Robot rw) = rw

-- Gold
newtype Gold = Gold { getGold :: (Sum Int) }
getGoldAmount = getSum . getGold
instance Show Gold where
  show (Gold (Sum n)) = show n ++ " Gold" 

instance Monoid Gold where
  mempty = Gold $ mempty       
  Gold g1 `mappend` Gold g2 = Gold $ g1 <> g2

-- Results
newtype Result = Result Bool
instance Show Result where
  show (Result b) = (if b then "something" else "nothing") ++ " interesting!"

gold :: Int -> Gold
gold = Gold  . Sum


-- actual Robot actions
-- TODO: more mining actions
idle :: Robot ()
idle = Robot $ return ()

mine :: Robot Gold 
mine = Robot $ do
  msg "mining"
  g <- findGold 
  msg $ "found " ++ show g
  return g
  where findGold = liftIO (fmap gold $ randomRIO (3, 15)) 


-- Replace Bool with a dedicated Scan datatype
scan :: Robot Result
scan = Robot $ do
  msg "scanning" 
  r <- fmap Result $ liftIO randomIO
  msg $ "detected: " ++ show r
  return r

-- Todo: no need to nest go
go :: Int -> Robot a -> Robot a
go dist (Robot c) = Robot $ do
  home <- get
  move dist
  r <- c
  return r

(>+>) :: Robot Gold -> Robot Gold -> Robot Gold
(>+>) (Robot c1) (Robot c2) = Robot $ liftM2 (<>) (c1) (c2)
(<->) :: Robot a -> Robot b -> Robot b 
(<->) (Robot c1) (Robot c2) = Robot $ (goHome $ Robot c1) >> goHome (Robot c2)

goHome ::  Robot a -> RWorld a 
goHome (Robot c) = do
  home <- getPos
  x <- c
  here <- getPos
  let way = (home - here)
  when (way /= 0) $ do
    move (home - here)
    dest <- getPos
    if dest == 0
    then msg $ "brought back: " ++ show x
    else msg $ "dumped: " ++ show x
  return x

timeRobot :: Robot a -> IO ()
timeRobot c = do
  t <- getCurrentTime 
  runRobot c
  t' <- getCurrentTime
  Prelude.putStrLn $ "I needed " ++ show (t' `diffUTCTime` t) ++ " seconds for my work"

-- >> examples
robprog     = (go 7 mine) <-> (go 5 mine) <->  (go 6 scan)
robprog'    = (go 7 (mine >+> go (-2) mine)) <-> (go 6 scan)
somemining  = (go 7 (mine >+> go (-2) mine)) 
-- >> end

