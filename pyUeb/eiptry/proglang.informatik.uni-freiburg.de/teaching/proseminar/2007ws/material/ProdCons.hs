import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

---------------- PRODUCER --------------------

prod :: TChan Int -> IO ()
prod c = 
    do x <- randomIO
       putStrLn ('>':show x)
       atomically (writeTChan c x)
       del <- randomIO
       threadDelay (del `mod` 5000000)
       prod c

--------------- CONSUMER ---------------------

cons :: TChan Int -> IO ()
cons c =
    do x <- atomically (readTChan c)
       putStrLn ('<':show x)
       cons c

------------- HAUPTPROGRAMM ------------------

nprods = 20

main :: IO ()
main =
    do pc1 <- newTChanIO
       pc2 <- newTChanIO
       forkIO $ prod pc1
       forkIO $ prod pc2

       cc <- newTChanIO
       forkIO $ cons cc

       merge pc1 pc2 cc


------------ HIER AUSFUELLEN -----------------

merge :: TChan Int   -- ^Eingang 1
      -> TChan Int   -- ^Eingang 2
      -> TChan Int   -- ^Ausgänge
      -> IO ()
merge ein1 ein2 aus =
    -- ?
