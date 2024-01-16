import Control.Concurrent

consumer :: MVar Int -> IO ()
consumer mvar = do putStrLn $ "Empfange einen Wert"
                   x <- takeMVar mvar
                   putStrLn $ "Empfangen: " ++ (show x)

producer :: MVar Int -> MVar () -> IO ()
producer mvar sig = 
    do putStrLn $ "Sende einen Wert"
       putMVar mvar 1
       putStrLn $ "Gesendet: 1"

main :: IO ()
main = do mvar <- newMVar 0
          sig <- newEmptyMVar
          takeMVar mvar
          forkIO (producer mvar sig)
          consumer mvar
