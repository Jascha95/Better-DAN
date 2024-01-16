-- TODO: define proper exports
module FS
       ( FS()
       , Message
       , createFile
       , deleteFile
       , readFile
       , writeFile
       , runFS
       , ex_fs1
       , ex_fs2
       , FSRep
       , fsEmpty
       , fsInsert
       , fsLookup)
       where
       
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class
import Control.Monad
import Data.Maybe
import Data.List (intercalate)
import Prelude hiding (readFile, writeFile, lookup)

newtype FSRep = FSRep { unFSRep :: (Map String String) }

instance Show FSRep where
  show (FSRep m) = format (M.assocs m)
    where format = show . map (\(k, v) -> k ++ " -> " ++ show v )

fsInsert :: String -> String -> FSRep -> FSRep
fsInsert k a  = FSRep . M.insert k a . unFSRep
fsLookup :: String -> FSRep -> Maybe String
fsLookup k    = M.lookup k . unFSRep
fsDelete :: String -> FSRep -> FSRep
fsDelete k    = FSRep . M.delete k . unFSRep
fsEmpty :: FSRep
fsEmpty       = FSRep M.empty
         
type Message = String
type FSM = ErrorT Message (State FSRep)
newtype FS a = FS { unFS ::  FSM a }
runFS :: FS a -> FSRep -> (Either Message a, FSRep)
runFS act s =  runState (runErrorT (unFS act)) s

instance Monad FS where 
  return = FS . return
  FS sa1 >>= f = FS $ sa1 >>= unFS . f



ifExists :: (String -> FSM a) -> FSM a -> String -> FSM a
ifExists thn els fn = do
  mf <- lift $ gets (fsLookup fn)
  maybe (els) thn mf

whenExists a fn = ifExists a (throwError msg) fn
  where msg = "File `" ++ fn ++ "' not found"

unlessExists a fn = ifExists (const (throwError msg)) a fn
  where msg = "File `" ++ fn ++ "' already exists"

assertExists fn = whenExists (const return ()) fn
assertNotExists fn = unlessExists (return ()) fn


readFile :: String -> FS String
readFile = FS . whenExists return 
writeFile :: String -> String -> FS ()
writeFile fn c = FS $ do
  assertExists fn
  lift $ modify (fsInsert fn c)
  where msg = "File `" ++ fn ++ "' not found"
  
createFile :: String -> FS ()
createFile fn = FS $ assertNotExists fn >> (lift (modify (fsInsert fn ""))) 

deleteFile :: String -> FS ()
deleteFile fn = FS $ assertExists fn >> lift (modify (fsDelete fn))

-- This example finishes successfully
ex_fs1 = do
  createFile "Hello.txt"
  writeFile "Hello.txt" "Hello World!"
  createFile "diary.txt" 
  writeFile "diary.txt" "Not much going on..."
  diary <- readFile "diary.txt"
  deleteFile "diary.txt"
  writeFile "Hello.txt" "Hello, again!"
  return diary

-- This example stops with an error
ex_fs2 = do
  createFile "Hello.txt"
  writeFile "Hello.txt" "Hello World!"
  createFile "diary.txt" 
  writeFile "diary2.txt" "Not much going on..."
  diary <- readFile "diary.txt"
  deleteFile "diary.txt"
  writeFile "Hello.txt" "Hello, again!"
