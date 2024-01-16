import Control.Concurrent
import Control.Monad

---------------- TESTPROGRAMM ------------------

writer :: BChan Int -> IO ()
writer bc =
    do mapM_ (\n -> do 
                writeBChan bc n
                putStrLn $ '>':(show n)
            ) [1..12]

reader :: BChan Int -> IO ()
reader bc =
    do replicateM_ 3 (do
                      threadDelay 3000000
                      replicateM_ 4 (do
                                     x <- readBChan bc
                                     putStrLn $ '<':(show x)
                                     ))
                                     
                        
                        

main :: IO ()
main =
    do bc <- newBChan 3 
       forkIO (writer bc)
       reader bc

-------------- HIER AUSFUELLEN -----------------

type BChan a = -- ?

newBChan :: Int -> IO (BChan a)
newBChan kapazitaet = 
    -- ?

readBChan :: BChan a -> IO a
readBChan bchan = 
    -- ?

writeBChan :: BChan a -> a -> IO ()
writeBChan bchan a =
    -- ?
       