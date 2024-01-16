> module Week7 where

> import Data.Monoid

Adapted from Phil Wadler's "Monads for Functional Programming"

* The art of writing an interpreter.
Let's write an interpreter for a simple language and extend it gradually.

** Variation 0: The base interpreter

Terms are built form integer constants and division.

> data Term = Con Int | Div Term Term
>   deriving (Eq, Show)

> eval1 :: Term -> Int
> eval1 (Con i) = i
> eval1 (Div t1 t2) = eval1 t1 `div` eval1 t2

Two example terms

> t_answer, t_error :: Term
> t_answer = Div (Div (Con 1972) (Con 2)) (Con 23)
> t_error  = Div (Con 1) (Con 0)


** Variation 1: Error messages

Instead of crashing the program at a division by zero, let's capture this error
and relay it to the programmer in a handleable way.

> data M2 t = Raise Exception | Return t
>   deriving (Eq, Show)
> type Exception = String

> eval2 :: Term -> M2 Int
> eval2 (Con i) = Return i
> eval2 (Div t1 t2) = 
>   case eval2 t1 of
>     Raise exc -> Raise exc
>     Return i1 -> 
>       case eval2 t2 of
>         Raise exc -> Raise exc
>         Return i2 -> if i2==0 then Raise "division by zero" else Return (i1 `div` i2)












** Variation 2: State

Let's count the number of divisions.

> type M3 t = State -> (t, State)
> type State = Int

> eval3 :: Term -> M3 Int
> eval3 (Con i) = \s -> (i, s)
> eval3 (Div t1 t2) = \s -> 
>   let (i1, s1) = eval3 t1 s
>       (i2, s2) = eval3 t2 s1
>   in  (i1 `div` i2, s2+1)



** Variation 3: Output

Let's create a trace of the execution alongside.

> type M4 t = (Output, t)
> type Output = String

> eval4 :: Term -> M4 Int
> eval4 t@(Con i) =
>   (show t ++ " = " ++ show i ++ "\n", i)
> eval4 t@(Div t1 t2) = 
>   let (o1, i1) = eval4 t1
>       (o2, i2) = eval4 t2
>       i        = i1 `div` i2
>   in  (show t ++ " = " ++ show i ++ "\n" ++ o2 ++ o1, i)

original:   in  (o1 ++ o2 ++ show t ++ " = " ++ show i ++ "\n", i)

How about producing the trace in reverse?












* Alternatively start with a monadic interpreter

> evalM :: Monad m => Term -> m Int
> evalM (Con i) = 
>   return i
> evalM (Div t1 t2) =
>   do i1 <- evalM t1
>      i2 <- evalM t2
>      return (i1 `div` i2)



** An error monad

M2 is a monad:

> instance Monad M2 where
>   return = Return
>   ma >>= f = 
>     case ma of 
>       Raise exc -> Raise exc
>       Return a  -> f a

> raise :: String -> M2 a
> raise = Raise


> evalM2 (Con i) = 
>   return i
> evalM2 (Div t1 t2) =
>   do i1 <- evalM2 t1
>      i2 <- evalM2 t2
>      if i2==0 then raise "division by zero" else return (i1 `div` i2)







** A state monad

> newtype M3' t = M3' { exM3' :: State -> (t, State) }

> instance Monad M3' where
>   return i = M3' (\s -> (i, s))
>   ma >>= f = M3' (\s ->
>      let (a, s') = exM3' ma s
>      in  exM3' (f a) s')

> runM3' :: M3' t -> (t, State)
> runM3' (M3' m) = m 0

> tick :: M3' ()
> tick = M3' (\s -> ((), s+1))

> -- evalM3 :: Monad m => Term -> m Int
> evalM3 (Con i) = 
>   return i
> evalM3 (Div t1 t2) =
>   do i1 <- evalM3 t1
>      i2 <- evalM3 t2
>      tick
>      return (i1 `div` i2)


** checkpoint: continue on 20131210

** An output monad (writer)

> newtype M4' a = M4' { exM4' :: (Output, a) }

> instance Monad M4' where
>   return a = M4' (mempty, a)
>   ma >>= f = let (out1, a) = exM4' ma
>                  (out2, b) = exM4' (f a)
>              in  M4' (out2 `mappend` out1, b)

> out :: Output -> M4' ()
> out o = M4' (o, ())

> runM4' :: M4' t -> (Output, t)
> runM4' = exM4'

> -- evalM4 :: Monad m => Term -> m Int
> evalM4 t@(Con i) = do
>   out (show t ++ " --> " ++ show i ++ "\n")
>   return i
> evalM4 t@(Div t1 t2) =
>   do i1 <- evalM4 t1
>      i2 <- evalM4 t2
>      let i0 = (i1 `div` i2)
>      out (show t ++ " --> " ++ show i0 ++ "\n")
>      return i0






** Adding variables - the environment monad (reader)

> data Term1 = Con1 Int | Div1 Term1 Term1 | Var Ident | Let Ident Term1 Term1
>   deriving (Eq, Show)
> type Ident = String
> type Env = Ident -> Maybe Int

> emptyEnv :: Env
> emptyEnv i = Nothing

> lookup :: Ident -> Env -> Maybe Int
> lookup i env = env i

> extend :: Env -> Ident -> Int -> Env
> extend env i v = \j -> if i==j then Just v else env j

> data EnvM t = EnvM { getEnvM :: Env -> t }

> instance Monad EnvM where
>   return x = EnvM $ \env -> x
>   ma >>= f = EnvM $ \env -> let a = getEnvM ma env in getEnvM (f a) env

> evalME :: Term1 -> EnvM Int
> evalME = undefined

