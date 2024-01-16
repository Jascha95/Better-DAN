module Logging where

import Control.Monad.Trans
import Control.Monad.Trans.Writer.Strict
import Data.Time.Clock.POSIX
    
data Item = Msg POSIXTime String
          | Section String POSIXTime POSIXTime [Item]
            deriving (Show,Eq)
type Log = [Item]

type Logging a = WriterT Log IO a
    
log :: Show t => t -> Logging ()
with_section :: String -> Logging a -> Logging a
runLogging :: Logging a -> IO (a, Log)

log s = do
  t <- lift getPOSIXTime
  tell [Msg t (show s)]

with_section s m = 
  pass $ do
    t1 <- lift getPOSIXTime
    x <- m
    t2 <- lift getPOSIXTime
    return (x, \l -> [Section s t1 t2 l])
    
runLogging x = runWriterT x
