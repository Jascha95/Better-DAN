data Term  = Con Integer
           | Bin Term Op Term  
             deriving (Eq, Show)
           
data Op    =  Add | Sub | Mul | Div
              deriving (Eq, Show)


{- Einfacher Interpreter -}

eval_simple              :: Term -> Integer
eval_simple (Con n)      =  n
eval_simple (Bin t op u) =  sys op (eval_simple t) (eval_simple u)

sys Add       =  (+)         
sys Sub       =  (-)
sys Mul       =  (*)         
sys Div       =  div


{- Einfacher Interpreter mit monadischer Notation -}

newtype Id a = Id a deriving (Show)

instance Monad Id where
    return x       = Id x
    x >>= f        = let Id y = x in
             	     f y

eval_monad                  :: Term -> Id Integer
eval_monad (Con n)          =  return n
eval_monad (Bin t op u)     =  eval_monad t  >>= \v ->
                               eval_monad u  >>= \w ->
                               return (sys op v w)


{- Interpreter mit Fehlerbehandlung -}

data Exception a =  Raise  String
                 |  Return a 
                 deriving (Show)


eval_ex              :: Term -> Exception Integer
eval_ex (Con n)      = Return n
eval_ex (Bin t op u) = case eval_ex t of
                        Raise  s -> Raise s
                        Return v -> case eval_ex u of
                                      Raise  s -> Raise s
                                      Return w ->
                                       if (op == Div && w == 0)
                                        then Raise "div by zero"
                                       else Return (sys op v w)

{- Monadischer Interpreter mit Fehlerbehandlung -}

instance Monad Exception where
  return a  =  Return a
  m >>= f   =  case m of 
                 Raise  s -> Raise s
                 Return v -> f v
  fail s    =  Raise s

eval_mex               :: Term -> Exception Integer
eval_mex (Con n)      = return n
eval_mex (Bin t op u) = eval_mex t  >>= \v ->
                        eval_mex u  >>= \w -> 
                        if (op == Div && w == 0) 
                         then fail "div by zero"
                        else return (sys op v w)

{- Interpreter mit Protokoll -}

newtype Trace a =  Trace (a, String) deriving (Show)

eval_prot :: Term -> Trace Integer
eval_prot e@(Con n)       = Trace (n, trace e n)
eval_prot e@(Bin t op u)  = let Trace (v, x) = eval_prot t in
                            let Trace (w, y) = eval_prot u in
                            let r = sys op v w in
                             Trace (r, x ++ y ++ trace e r)
 
trace t n = "eval (" ++ show t ++ ") = "
              ++ show n ++ "\n"


{- Monadischer Interpreter mit Protokoll -}

instance Monad Trace where
  return a = Trace (a, "")
  m >>= f  = let Trace (a, x) = m in
             let Trace (b, y) = f a in
              Trace (b, x ++ y)

output   :: String -> Trace ()
output s  = Trace ((), s)

eval_mprot :: Term -> Trace Integer
eval_mprot e@(Con n)      = output (trace e n) >>
                       return n
eval_mprot e@(Bin t op u) = eval_mprot t >>= \v ->
                            eval_mprot u >>= \w ->
                            let r = sys op v w in
                             output (trace e r) >>
                             return r


{- Interpreter mit Reduktionszaehler -}

type Count a = Int -> (a,Int)

eval_state :: Term -> Count Integer
eval_state (Con n)      = \i -> (n,i)
eval_state (Bin t op u) = \i -> let (v,j) = eval_state t i in
                                let (w,k) = eval_state u j in
                                 (sys op v w, k + 1)

{- Monadischer Interpreter mit Reduktionszaehler -}

data ST s a = ST (s -> (a,s)) 
exST (ST sas) = sas

instance Monad (ST s) where
  return a = ST (\s -> (a,s))
  m >>= f  = ST (\s -> let (a,s') = exST m s in
                        exST (f a) s')

type CountST a = ST Int a 

incr :: CountST ()
incr = ST (\i -> ((), i + 1))

eval_mstate :: Term -> CountST Integer
eval_mstate (Con n)      = return n
eval_mstate (Bin t op u) = eval_mstate t >>= \v ->
                           eval_mstate u >>= \w ->
                           incr >> 
                           return (sys op v w)


test_t = Bin (Con 1) Add (Con 1)
