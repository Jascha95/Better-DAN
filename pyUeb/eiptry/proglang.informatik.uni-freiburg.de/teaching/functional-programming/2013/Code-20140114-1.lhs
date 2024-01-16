> module Week1403 where
> import Control.Monad

* Arrows - Motivation

This lecture is based on John Hughes's arrow tutorial
Programming With Arrows
http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf

Many Haskell programmers enjoy point-free definition like this one, which counts the number of occurrences of a word in a string.

> count w = length . filter (==w) . words

However, the corresponding program to count the words in a file cannot easily be written in a point-free style. In particular, the following does not type check:

| countFile w = print . length . filter (==w) . words . readFile

... because readFile and print are in the IO monad.
A Little bit of combinator gymnastics helps:

> countFile w = (>>= print) . liftM (length . filter (==w) . words) . readFile

But there is a better approach. Let's define the type of arrows in the Kleisli category over a monad.

> type Kleisli m a b = a -> m b

With this definition, we can see that readFile and print are both Kleisli arrows.

| readFile :: Kleisli IO String String
| print    :: Show a => Kleisli IO a ()

Next, define Kleisli composition:

> (>>>) :: (Functor m, Monad m) => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
> -- (f >>> g) a = f a >>= g
> f >>> g = join . fmap g . f



> printFile = readFile >>> print

... but in the original problem, there were also effect-free functions that needed to be composed with the Kleisli arrows. Solution: we lift standard arrows to Kleisli arrows.

> arr :: Monad m => (a -> b) -> Kleisli m a b
> arr f = return . f

Now, we can give a convenient point-free definition:

> countK w = readFile >>> arr words >>> arr (filter (==w)) >>> arr length >>> print

