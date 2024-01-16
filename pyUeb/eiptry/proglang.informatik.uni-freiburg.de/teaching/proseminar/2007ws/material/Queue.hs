import Prelude hiding (head,tail)

data Queue a = Q [a] [a] deriving Show

check [] r = Q (reverse r) []
check f r  = Q f r

isEmpty (Q f r) = null f

snoc (Q f r) x = check f (x:r)

head (Q [] _) = error "empty Q"
head (Q (x:f) r) = x

tail (Q [] _) = error "empty Q"
tail (Q (x:f) r) = check f r
