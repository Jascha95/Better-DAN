import Data.List

readFiles f1 f2 =
   readFile f1 >>= \xs1 -> readFile f2 >>= \xs2 -> return (xs1, xs2)

readFiles' f1 f2 = do
  xs1 <- readFile f1
  xs2 <- readFile f2
  return (xs1, xs2)

copyFile :: FilePath -> FilePath -> IO ()
copyFile source target = do
  contents <- readFile source
  writeFile target contents

-- doTwice :: IO a -> IO (a,a)
doTwice io = do
  x1 <- io
  x2 <- io
  return (x1, x2)

-- doNot :: IO a -> IO ()
doNot io = do
  return ()

{-
(>>) :: Monad m => m a -> m b -> m b
ma >> mb = do 
  ma
  mb

ma >> mb = ma >>= \_ -> mb
-}

sortFile :: FilePath -> FilePath -> IO ()
sortFile source target = do 
  xs <- readFile source
  (writeFile target . unlines . sort . lines) xs
  -- writeFile target $ unlines $ sort $ lines xs

sortFile' source target =
  readFile source >>= (writeFile target . unlines . map (reverse . sort) . lines)

mysequence :: [IO a] -> IO [a]
mysequence [] = return []
mysequence (io:ios) = do
  r <- io
  rs <- mysequence ios
  return (r:rs)

