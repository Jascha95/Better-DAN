module Logging where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) =
  if pass == s then Just v else Nothing

-- v1
     
-- type Protected s a = MaybeT (Reader (ProtectedData s)) a

-- run :: ProtectedData s -> Protected s a -> Maybe a
-- run s m = runReader (runMaybeT m) s
    
-- access :: String -> Protected a a
-- access pass = do
--   protectedData <- lift $ ask
--   let v = accessData pass protectedData
--   MaybeT $ return v

-- v2
     
type Protected s a = MaybeT (ReaderT (ProtectedData s) IO) a

run :: ProtectedData s -> Protected s a -> IO (Maybe a)
run s m = runReaderT (runMaybeT m) s
    
access :: Protected a a
access = do
  lift $ lift $ print "Please enter your password."
  pass <- lift $ lift $ getLine
  protectedData <- lift $ ask
  let v = accessData pass protectedData
  MaybeT $ return v
  
