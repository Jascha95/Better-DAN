> module Week1403_1 where
> import Control.Monad

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

> delay x = SF (x:)

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


> class Arrow1 arr => Arrow arr where
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

> ite p f g = (p &&& arr id) >>> arr isoBoolA >>> (f ||| g)

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



For stream functions, we need to restrict to length preserving
functions.

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
>   loop f a = b
>     where (b,c) = f (a,c)

What about the instances for Kleisli and stream functions?

