module Numbers where

main :: IO ()
main = do
  putStrLn "Pick a number between 1 and 100"
  game 1 100
    
game :: Int -> Int -> IO ()
game i j =
  if i == j then
    putStrLn $ "It must be "++ show i ++"."
  else do
    let n = (i+j) `div` 2
    putStrLn $ "Is it "++ show n ++"?"
    askInput n

    where
      askInput n = do
         s <- getLine
         case s of
           "greater" -> game (n+1) j
           "smaller" -> game i (n-1)
           "yes" -> putStrLn "Yay, I won!"
           _ -> do
             putStrLn "The answer must be greater, smaller or yes."
             askInput n
            
    
    
    
