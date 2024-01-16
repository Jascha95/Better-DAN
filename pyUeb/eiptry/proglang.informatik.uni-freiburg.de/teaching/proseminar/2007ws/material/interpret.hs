data Term  = Con Integer
           | Bin Term Op Term  
             deriving (Eq, Show)
           
data Op    =  Add | Sub | Mul | Div
              deriving (Eq, Show)

eval0              :: Term -> Integer
eval0 (Con n)      =  n
eval0 (Bin t op u) =  sys op (eval0 t) (eval0 u)

sys Add       =  (+)         
sys Sub       =  (-)
sys Mul       =  (*)         
sys Div       =  div

newtype Id a = Id a deriving (Show)

instance Monad Id where
    return x       = Id x
    x >>= f        = let Id y = x in
             	     f y

eval1                  :: Term -> Id Integer
eval1 (Con n)          =  return n
eval1 (Bin t op u)     =  eval1 t  >>= \v ->
                          eval1 u  >>= \w ->
                          return (sys op v w)



data Exception a =  Raise  String
                 |  Return a deriving (Show)


eval3              :: Term -> Exception Integer
eval3 (Con n)      = Return n
eval3 (Bin t op u) = case eval3 t of
                     Raise  s -> Raise s
                     Return v -> case eval3 u of
                      Raise  s -> Raise s
                      Return w ->
                        if (op == Div && w == 0)
                        then  
                          Raise "div by zero"
                        else                                  
                          Return (sys op v w)

instance Monad Exception where
  return a  =  Return a
  m >>= f   =  case m of 
                 Raise  s -> Raise s
                 Return v -> f v
  fail s    =  Raise s

eval4               :: Term -> Exception Integer
eval4 (Con n)      = return n
eval4 (Bin t op u) = eval4 t  >>= \v ->
                    eval4 u  >>= \w ->
                    if (op == Div && w == 0) 
                     then fail "div by zero"
                    else return (sys op v w)

newtype Trace a =  Trace (a, String) deriving (Show)

eval7 :: Term -> Trace Integer
eval7 e@(Con n)       = Trace (n, trace e n)
eval7 e@(Bin t op u)  = let Trace (v, x) = eval7 t in
                        let Trace (w, y) = eval7 u in
                        let r      = sys op v w in
                        Trace (r, x ++ y ++ trace e r)

trace t n = "eval (" ++ show t ++ ") = "
              ++ show n ++ "\n"

instance Monad Trace where
  return a = Trace (a, "")
  m >>= f  = let Trace (a, x) = m in
             let Trace (b, y) = f a in
             Trace (b, x ++ y)

output   :: String -> Trace ()
output s  = Trace ((), s)
eval8 :: Term -> Trace Integer
eval8 e@(Con n) = output (trace e n) >>
                 return n
eval8 e@(Bin t op u) = eval8 t >>= \v ->
                       eval8 u >>= \w ->
                       let r = sys op v w in
                       output (trace e r) >>
                       return r

test_t = Bin (Con 1) Add (Con 1)
