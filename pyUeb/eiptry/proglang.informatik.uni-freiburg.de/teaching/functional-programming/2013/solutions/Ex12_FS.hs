{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances, DeriveFunctor #-}
module Ex12_FS where
import DTC
import Prelude hiding (readFile)
import qualified FS 
import FS (FS)

data FSWrite a = FSWrite String String a
  deriving Functor
data FSRead a = FSRead String (String -> a)
  deriving Functor
data FSDir a = Create String a | Delete String a
  deriving Functor

write :: (FSWrite :<: f) => String -> String -> Term f ()
write fn s = injectT (FSWrite fn s (Pure ()))
readFile :: (FSRead :<: f) => String -> Term f String
readFile fn = injectT (FSRead fn Pure)
create :: (FSDir :<: f) => String -> Term f ()
create fn = injectT (Create fn (Pure ()))
delete :: (FSDir :<: f) => String -> Term f ()
delete fn = injectT (Delete fn (Pure ()))

ex_createHello :: (Functor f, FSDir :<: f, FSWrite :<: f) => Term f ()
ex_createHello = create "Hello.txt"  >> write "Hello.txt" "Hello World!"

ex_fsfree1 :: Term (FSDir :+: FSRead :+: FSWrite) ()
ex_fsfree1 = do
  ex_createHello
  write "Hello.txt" "Hello World!"
  create "diary.txt" 
  write "diary2.txt" "Not much going on..."
  diary <- readFile "diary.txt"
  delete "diary.txt"
  write "Hello.txt" "Hello, again!"

class RunFS f where
  runFsAlg :: f (FS a) -> FS a

instance (RunFS f, RunFS g) => RunFS (f :+: g) where
  runFsAlg (Inl f) = runFsAlg f
  runFsAlg (Inr g) = runFsAlg g

instance RunFS FSDir where
  runFsAlg (Create s x) = FS.createFile s >> x
  runFsAlg (Delete s x) = FS.deleteFile s >> x

instance RunFS FSRead where
  runFsAlg (FSRead s f) = FS.readFile s >>= f  

instance RunFS FSWrite where
  runFsAlg (FSWrite s v x) = FS.writeFile s v >> x
runFS tm = FS.runFS $ foldTerm return runFsAlg tm 

ex_tm1 :: Term (FSDir :+: FSRead :+: FSWrite) String
ex_tm1 = create "1.txt" >> create "2.txt" >>
         write "2.txt" "2" >> delete "1.txt" >> readFile "2.txt"

-- Die Beispiele vom Blatt:
ex_fs1 :: Term (FSDir :+: FSRead :+: FSWrite) String
ex_fs1 = do
   create "Hello.txt" 
   write "Hello.txt" "Hello World!"
   create "diary.txt"
   write "diary.txt" "Not much going on..."
   diary <- readFile "diary.txt"
   delete "diary.txt"
   write "Hello.txt" "Hello, again!"
   return diary

ex_fs2 :: Term (FSDir :+: FSRead :+: FSWrite) ()
ex_fs2 = do
  create "Hello.txt"
  write "Hello.txt" "Hello World!"
  create "diary.txt" 
  write "diary2.txt" "Not much going on..."
  diary <- readFile "diary.txt"
  delete "diary.txt"
  write "Hello.txt" "Hello, again!"

