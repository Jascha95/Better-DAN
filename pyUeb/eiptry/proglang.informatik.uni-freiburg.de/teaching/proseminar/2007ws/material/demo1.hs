import Control.Concurrent

putChars :: Char -> IO ()
putChars c = do putChar c
                putChars c

main :: IO ()
main = do forkIO (putChars 'a')
          putChars 'b'
