> module Week1403_1 where
> import Control.Monad
> import Control.Monad.Fix


* The Arrow class

Generalizing this development leads to the arrow class.

> class Arrow0 arr where
>   arr :: (a -> b) -> arr a b
>   (>>>) :: arr a b -> arr b c -> arr a c

> instance Arrow0 (->) where
>   arr f = f
>   f >>> g = g . f

A trivial arrow instance: functions.

 > instance Arrow0 (->) where
 >   arr = id
 >   (>>>) = flip (.)

To make Kleisli arrows an instance, they need to be made a newtype.

> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

> instance (Functor m, Monad m) => Arrow0 (Kleisli m) where
>   arr f = Kleisli (return . f)
>   f >>> g = Kleisli (join . fmap (runKleisli g) . runKleisli f)

The counter may now be programmed.

> countA w = Kleisli readFile >>>
>            arr words >>> arr (filter (==w)) >>> arr length >>>
>            Kleisli print

> runcount w filename = runKleisli (countA w) filename

Another instance.

Stream transformers model discrete time varying behavior.

> newtype SF a b = SF { runSF :: [a] -> [b] }

> instance Arrow0 SF where
>   arr f = SF (map f)
>   f >>> g = SF (runSF f >>> runSF g)


Arrow types are mostly useful for the additional operations that they support.

> delay' x = SF (x:)

Another instance.

State and behavior transformers.

> newtype SB s a b = SB { runSB :: (s -> a) -> (s -> b) }

> instance Arrow0 (SB s) where
>   arr f = SB (f .)
>   f >>> g = SB (runSB f >>> runSB g)

f :: a -> b
(s -> a) -> (s -> b)


* Laws

In analogy to the monad laws, there are also arrow laws.
However, there are twenty of them stated in the original paper and
another seven laws in a followup paper. A recent paper ("The arrow
calculus" by Phil Wadler and others) brings this down to a much
smaller number. But it would require learning the arrow calculus.

* Arrows and pairs

The arrow interface introduced up to now cannot handle passing
multiple arguments to a function. With a monad it is quite
straightforward to compute to numbers and multiply them:

> mulM :: Monad m => m Int -> m Int -> m Int
> mulM a b = do { x <- a; y <- b; return (x * y) }

But if we want to do the same with arrows, we fail:

> mulA0 :: Arrow0 arr => arr a Int -> arr a Int -> arr a Int
> mulA0 f g = undefined

It's unclear how to compose f and g: f consumes the input a and yields
an Int. But then, the >>> operator requires an arrow with output a to
compose with g. So a further operation is needed to `pair up' two
arrows.

> class Arrow0 arr => Arrow1 arr where
>   (&&&) :: arr a b -> arr a c -> arr a (b,c)

> mulA :: Arrow1 arr => arr a Int -> arr a Int -> arr a Int
> mulA f g = f &&& g >>> arr (uncurry (*))

We need to extend the existing instances...

> instance Arrow1 (->) where
>   f &&& g = \a -> (f a, g a)

> instance (Monad m, Functor m) => Arrow1 (Kleisli m) where
>   f &&& g = Kleisli (\a -> do b <- runKleisli f a
>                               c <- runKleisli g a
>                               return (b,c))

f :: a -> m b
g :: a -> m c
a -> m (b,c)


> instance Arrow1 SF where
>   f &&& g = SF ((runSF f &&& runSF g) >>> uncurry zip)

first parenthesis :: ([b], [c])
wanted :: [(b, c)]

An example of a stream function.

> pairPred = arr id &&& delay 0

A simpler way to make arrows and pairs interact.

> (***) :: Arrow arr => arr a b -> arr c d -> arr (a,c) (b,d)


> class Arrow0 arr => Arrow arr where
>   first :: arr a b -> arr (a,c) (b,c)

We can define second that works on the second component from first:

> second :: Arrow arr => arr a b -> arr (c,a) (c,b)
> second f = arr swap >>> first f >>> arr swap

> swap (x,y) = (y,x)

Now (***) becomes defineable:

> f *** g = first f >>> second g

We can express &&& in terms of ***:

> diag :: Arrow arr => arr a (a,a)
> diag = arr (\x -> (x,x))

> f &&&& g = diag >>> (f *** g)

> instance Arrow (->) where
>   first f = \(a,c) -> (f a,c)


> instance (Functor m, Monad m) => Arrow (Kleisli m) where
>   first f = Kleisli (\(a, c) -> do { b <- runKleisli f a; return (b,c)})

> instance Arrow SF where
>   first f = SF (unzip >>> first (runSF f) >>> uncurry zip)

f :: [a] -> [b]
first f :: [(a,c)] -> [(b,c)]


* Arrows and conditionals

So far, arrows do not interact well with conditionals. To build an
arrow that depends on the result of a previous arrow requires further
combinators. Such a combinator might look like this:

> ite :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b

However, its implementation may be built from simpler parts.

> ite p f g = (p &&&& arr id) >>> arr isoBoolA >>> (f ||| g)

The bool type is too specific.

> isoBoolA :: (Bool, a) -> Either a a
> isoBoolA (True, a) = Left a
> isoBoolA (False, a) = Right a

The choice operator, version 1.

> (|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c

This signature is dual to (&&&)'s type.
Let's explore duality further and find the dual to (***) and first
and make them part of the definition of an extended arrow class:

> (+++) :: ArrowChoice arr => arr a b -> arr c d -> arr (Either a c) (Either b d)

The first choice operator becomes defineable:

> f ||| g = f +++ g >>> arr (either id id)

But again, there is a simpler combinator underlying (+++) that becomes
the basis of the extended type class.

> class Arrow arr => ArrowChoice arr where
>   left :: arr a b -> arr (Either a c) (Either b c)

Its mirror image is easily definable.

> right :: ArrowChoice arr => arr a b -> arr (Either c a) (Either c b)
> right f = arr mirror >>> left f >>> arr mirror

> mirror (Left x) = Right x
> mirror (Right y) = Left y

And we can complete the definition for the second choice operator.

> f +++ g = left f >>> right g

It remains to implement instances of ArrowChoice for the examples.

> instance ArrowChoice (->) where
>   left f = either (Left . f) Right -- Either a c

For the instance for the Kleisli arrow consider the typing of f:

f :: a -> M b
result :: (Either a c) -> M (Either b c)

> instance (Monad m, Functor m) => ArrowChoice (Kleisli m) where
>   left f = Kleisli $ either (liftM Left . runKleisli f) (return . Right)

For stream functions, we need to restrict to length preserving
functions.

f :: [a] -> [b]
result :: [Either a c] -> [Either b c]

> instance ArrowChoice SF where
>   left f = SF (\eacs -> let fbs = runSF f [ a | Left a <- eacs ]
>                             combine [] fbs = []
>                             combine (Left _: rest) (fb:fbs) = Left fb: combine rest fbs
>                             combine (Right c: rest) fbs = Right c: combine rest fbs
>                         in  combine eacs fbs)

Length-preservation is necessary to fulfill the arrow laws.
Fixing delay (which was not length-preserving).

> delay x = SF (init . (x:))

Task: implement

> mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
> mapA f = arr listcase >>>
>          (arr (const []) ||| (f *** mapA f >>> arr (uncurry (:))))

> listcase :: [a] -> Either () (a, [a])
> listcase [] = Left ()
> listcase (x:xs) = Right (x,xs)


* Synchronous circuits

A nor-gate

> nor :: SF (Bool, Bool) Bool
> nor = arr (not . uncurry (||))

Rising edge detection: compare the signal with a one-step-delayed copy

> edge :: SF Bool Bool
> edge = arr id &&& delay False >>> arr detect
>   where detect (a,b) = a && not b

It is well-known that connecting nor-gates suitably results in a
flip-flop. To represent that, we need to feed back the result of one
arrow into its own input. A new combinator is required for this task.

> class Arrow arr => ArrowLoop arr where
>   loop :: arr (a,c) (b,c) -> arr a b

> instance ArrowLoop (->) where
>   loop f a = fst p
>     where p = f (a,snd p)

| c = (fix (\c -> snd (f (a,c))))

What about the instances for Kleisli and stream functions?

For the Kleisli instance, a monad with fixpoint is required.

For the SF instance, some care is needed to obtain a terminating definition.

f :: [(a,c)] -> [(b,c)]
result :: [a] -> [b]

> instance ArrowLoop SF where
>   loop f = SF (\as -> let (bs, cs) = unzip (runSF f (zip as (stream cs))) in bs)
>     where stream ~(x:xs) = x:stream xs

From these constitutents, we can build the flipflop.

> flipflop = 
>   loop (arr (\((reset,set),~(c,d)) -> ((set,d),(reset,c))) >>>
>         (nor *** nor) >>>
>         delay (False,True) >>>
>         (arr id &&& arr id))

* Applicable arrows

Would it make sense to reify arrows as functions and apply them?
Clearly, such an operation is not applicable to all kinds of
arrows. Hence, we define another type class that captures the arrows
with a suitable notion of `arrow application'.

> class Arrow arr => ArrowApply arr where
>   app :: arr (arr a b, a) b

For the standard function arrows as well as for the Kleisli arrows,
this function is implementable.

> instance ArrowApply (->) where
>   app = uncurry ($)

> instance (Monad m, Functor m) => ArrowApply (Kleisli m) where
>   app = Kleisli (\(kf,x) -> runKleisli kf x)

However, app is not implementable for stream functions.

Two observations:
* first and left can be implemented using app, so app is stronger as a
primitive. 
* a monad can be constructed from any arrow supporting app.

> newtype ArrowMonad arr a = ArrowMonad { exAM :: arr () a }

> instance ArrowApply a => Monad (ArrowMonad a) where
>   return x = ArrowMonad (arr (const x))
>   ArrowMonad m >>= f =
>     ArrowMonad (m >>>
>                 (arr (\x -> (exAM (f x), ()))) >>>
>                 app)

