> module Week6 where
> import Control.Monad

* Parsing

> data Expr = ETrue | ENot Expr | EOr Expr Expr

A parser maps a list of tokens to a result.

> type ParserV1 token result = [token] -> result
> type ExprParser = ParserV1 Char Expr

A parser may not consume the entire input.

> type ParserV2 token result = [token] -> (result, [token])

A parser may be non-deterministic:
Consider the grammar

A -> aA
A -> aB
B -> b

Represent using "list of successes" technique

> type Parser token result = [token] -> [(result, [token])]

parser recognizes ts if
parser ts   contains   (r, [])
==> all input ts has been consumed and the result is r

* Combinator parser
** Primitive parsers

empty recognizes the empty language

> empty :: Parser t r
> empty ts = []

succeed recognizes the empty word

> succeed :: r -> Parser t r
> succeed r ts = [(r, ts)]

satisfy p   recognizes   [t]  if  (p t)  holds

> satisfy :: (t -> Bool) -> Parser t t
> satisfy p [] = []
> satisfy p (t:ts) | p t       = [(t, ts)]
>                  | otherwise = []

lit t     recognizes   [t] 

> lit :: Eq t => t -> Parser t t
> lit t = satisfy (== t)

** Combination of parsers

Alternative <|> recognizes the union of two languages

> palt :: Parser t r -> Parser t r -> Parser t r
> palt p1 p2 = \ts -> p1 ts ++ p2 ts

Sequence <*> recognizes the concatenation of two languages

> pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
> pseq p1 p2 ts = [ (f s, ts'') | (f, ts') <- p1 ts, (s, ts'') <- p2 ts' ]

Semantic: pmap f p  recognizes  the same language as p,
          but changes the result

> pmap :: (s -> r) -> Parser t s -> Parser t r
> pmap f p ts = [ (f s, ts') | (s, ts') <- p ts]

** the example grammar

A -> aA
A -> aB
B -> b

> parseB = pmap (\x->[x]) $ lit 'b'
> parsea = pmap (:) $ lit 'a'  -- ('a':)
> parseA = palt (pseq parsea parseA) (pseq parsea parseB)

A -> aA
A -> aB
B -> bB
B -> 

> parseb = pmap (:) $ lit 'b' 
> parseB' = palt (pseq parseb parseB') (succeed "")
> parseA' = palt (pseq parsea parseA') (pseq parsea parseB')




* An application to parsing expressions

Consider the grammar

E -> E "+" T
E -> T
T -> "let" Ident "=" E "in" E
T -> Ident
T -> Number
T -> "(" E ")"

A datatype to represent the result of parsing
Transform the grammar; eliminate left recursion
Lexer...






* Parsers have rich structure ...
- many concepts from category theory can be mapped to programming concepts
- parsing fits many of these concepts

* Parsing is a functor

instance Functor (Parser t) where
  fmap f m = undefined

- functorial laws





* Parsing is a monad

instance Monad (Parser t) where
  return x = undefined
  ma >>= f = undefined

- monad laws

* Parsing is a monad with summation

instance MonadPlus (Parser t) where
  mzero = undefined
  mplus m1 m2 = undefined

* Parsing is applicative

** Origin: applicative programming with effects (McBride, Paterson)

- example 1 : sequencing computation

sequence :: [IO a] -> IO [a]

- example 2 : list of list transposition

> transpose :: [[a]] -> [[a]]
> transpose [] = repeat []
> transpose (xs:xss) = zipWith (:) xs (transpose xss)

> zapp :: [a -> b] -> [a] -> [b]
> zapp = undefined

zipWith_n :: (a1 -> a2 -> ... -> an -> b) -> [a1] -> [a2] -> ... -> [an] -> [b]

- example 3 : interpretation

> data Exp v
>   = Var v
>   | Val Int
>   | Add (Exp v) (Exp v)

> eval :: Exp v -> Env v -> Int
> eval (Var v) env = fetch v env
> eval (Val i) env = i
> eval (Add e1 e2) env = eval e1 env + eval e2 env

> eval' :: Exp v -> Env v -> Int
> eval' (Var v) = fetch v
> eval' (Val i) = const i
> eval' (Add e1 e2) = const (+) `ess` (eval' e1) `ess` (eval' e2)

> ess a b c = (a c) (b c)

> type Env v = v -> Int
> fetch :: v -> Env v -> Int
> fetch v env = env v

- examples share a common structure

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

** applicative laws
- Identity
pure id <*> v == v
- composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
- homomorphism
pure f <*> pure x = pure (f x)
- interchange
u <*> pure y = pure ($ y) <*> u

** further derived members of Applicative
(*>) :: f a -> f b -> f b

(<*) :: f a -> f b -> f a

