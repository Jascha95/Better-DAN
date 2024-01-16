> module Week8 where

We have seen how to use monads to express various computations.
What if we want to combine different computations modularly?

One possibility: monad transformers.

> class MonadTrans t where
>   lift :: Monad m => m a -> t m a

A monad transformer t takes a monad m and yield a new monad (t m).
The lift function serves to lift a computation from the underlying monad to the new monad.
Starting from a base monad, a monad may be constructed by composing transformers.





** Introducing doubt: the MaybeT transformer

> newtype MaybeT m a = MaybeT { exMaybeT :: m (Maybe a) }

> instance Monad m => Monad (MaybeT m) where
>   return a = MaybeT $ return $ Just a
>   MaybeT mma >>= f = MaybeT $ mma >>= \ma ->
>                      case ma of
>                      Nothing -> return Nothing
>                      Just a ->  exMaybeT (f a)

> mfail :: Monad m => MaybeT m a
> mfail = MaybeT $ return Nothing

instance MonadPlus














** Adding an environment

> newtype ReaderT r m a = ReaderT { exReaderT :: r -> m a }

> instance Monad m => Monad (ReaderT r m) where
>   return a = ReaderT $ \r -> return a
>   ReaderT rma >>= f = ReaderT $ \r -> rma r >>= \a -> exReaderT (f a) r

> instance MonadTrans (ReaderT r) where
>   -- lift :: m a -> t m a
>   lift ma = ReaderT $ \r -> ma

Operations on the environment monad: 

Retrieves the monad environment.

> ask :: Monad m => ReaderT r m r
> ask = ReaderT $ \r -> return r

Runs computation in modified environment.

> local :: Monad m => (r -> r) -> ReaderT r m a -> ReaderT r m a
> local f (ReaderT rma) = ReaderT $ (rma . f)








** Introducing state: the StateT transformer

> newtype StateT s m a = StateT { exStateT :: s -> m (a, s) }

> instance Monad m => Monad (StateT s m) where
>   return a = StateT $ \s -> return (a, s)
>   StateT sma >>= f = StateT $ \s -> sma s >>= \(a, s') -> exStateT (f a) s'

> instance MonadTrans (StateT s) where
>   lift ma = StateT $ \s -> ma >>= \a -> return (a, s)

Operations on the state monad: get and put

> get :: Monad m => StateT s m s
> get = StateT $ \s -> return (s, s)

> put :: Monad m => s -> StateT s m s
> put s' = StateT $ \s -> return (s', s')
















** Adding output

> newtype WriterT w m a = WriterT { exWriterT :: m (a, w) }

tell :: w -> m ()

listen :: m a -> m (a, w)

pass :: m (a, w -> w) -> m a

** composing a monad for an interpreter

base monad: the identity monad

> newtype I a = I a
> instance Monad I where
>   return a = I a
>   I a >>= f = f a

Features of the interpreter: 
** values
** exceptions: maybe
** variables: reader
** references: state

> type Env = [(Ident, Int)]

> type InterpM a = ReaderT Env (StateT Int (MaybeT I)) a
> run :: Show a => InterpM a -> String
> run (ReaderT rsma) = runS $ rsma []
> runS (StateT sma) = runMI $ sma 0
> runSXXX (StateT sxxxa) = runRMI $ sxxxa 0
> runRMI (ReaderT rmma) = runMI $ rmma []
> runMI (MaybeT mma) = runI mma
> runI (I a) = show a

> type Ident = String
> data Term = Con Int | Div Term Term
>           | Var Ident | Let Ident Term Term
>           | Read | Write Term
>           deriving (Show)

> eval :: Term -> InterpM Int
> eval (Con i) = 
>   return i
> eval (Div t1 t2) = do
>   i1 <- eval t1
>   i2 <- eval t2
>   if i2==0 then lift $ lift $ mfail else return $ i1 `div` i2
> eval (Var x) = do
>   env <- ask
>   maybe (lift $ lift mfail) return (lookup x env)
> eval (Let x t1 t2) = do
>   i1 <- eval t1
>   local ((x, i1):) $ eval t2
> eval (Read) = do
>   lift $ get
> eval (Write t1) = do
>   i1 <- eval t1
>   lift $ put i1
