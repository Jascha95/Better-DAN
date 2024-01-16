import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

type Gabel = TVar Bool
type Gabeln = [Gabel]

tisch :: Int -> STM Gabeln
tisch groesse =
  replicateM groesse (newTVar True)

essen :: Int -> Gabeln -> IO ()
essen n tisch = 
  do putStrLn $ "Philosoph " ++ (show n) ++ " hat Hunger"
     atomically (holeGabeln n tisch)
     putStrLn $ "Philosoph " ++ (show n) ++ " isst"
     randomIO >>= (threadDelay . (`mod`20000000))
     atomically (legeGabeln n tisch)

holeGabeln :: Int -> Gabeln -> STM ()
holeGabeln n tisch =
  do holeGabel n tisch
     holeGabel ((n + 1) `mod` (length tisch)) tisch

holeGabel :: Int -> Gabeln -> STM ()
holeGabel n tisch =
  do liegt <- readTVar (tisch !! n)
     if liegt then
       writeTVar (tisch !! n) False
      else retry

legeGabeln :: Int -> Gabeln -> STM ()
legeGabeln n tisch =
  do writeTVar (tisch !! n) True
     writeTVar (tisch !! ((n + 1) `mod` (length tisch))) True

denken :: Int -> IO () 
denken n =
  do putStrLn $ "Philosoph " ++ (show n) ++ " denkt"
     randomIO >>= (threadDelay . (`mod`20000000))

philosoph :: Int -> Gabeln -> IO ()
philosoph n tisch =
  do denken n
     essen n tisch
     philosoph n tisch

main :: IO ()
main =
  do t <- atomically (tisch 5)
     philosophen <- mapM_ (\n -> forkIO (philosoph n t)) [0..3]
     philosoph 4 t
