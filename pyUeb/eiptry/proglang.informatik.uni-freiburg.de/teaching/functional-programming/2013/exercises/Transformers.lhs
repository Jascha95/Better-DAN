> module Transformers where
> import Data.Monoid 

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


> newtype ErrorT a m b = ErrorT { exErrorT :: m (Either a b) }

> instance Monad m => Monad (ErrorT a m) where
>   return a = ErrorT $ return $ Right a
>   ErrorT mma >>= f = ErrorT $ mma >>= \ma ->
>                      case ma of
>                      Left a -> return $ Left a
>                      Right a ->  exErrorT (f a)

> instance MonadTrans (ErrorT a) where
>   lift a = ErrorT $ (return . Right) =<< a
                       
> mthrow :: Monad m => a -> ErrorT  a m b
> mthrow x = ErrorT $ return $ Left x












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
> instance (Monoid w) => MonadTrans (WriterT w) where
>   lift m =   WriterT $ m >>= \a -> return (a, mempty)
> instance (Monad m, Monoid w) => Monad (WriterT w m) where
>   return x = WriterT $ return (x, mempty) 
>   (WriterT m) >>= f  = WriterT $ do
>     (a, w) <- m
>     let WriterT m' = f a
>     (r, w') <- m'
>     return (r, w <> w')

> tell :: (Monad m, Monoid w) => w -> WriterT w m ()
> tell w = WriterT $ return ((), w)

listen :: m a -> m (a, w)

pass :: m (a, w -> w) -> m a

** composing a monad for an interpreter

base monad: the identity monad

> newtype I a = I { exI :: a }
> instance Monad I where
>   return a = I a
>   I a >>= f = f a


