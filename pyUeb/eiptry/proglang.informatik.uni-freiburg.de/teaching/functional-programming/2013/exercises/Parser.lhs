> module Parser where
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

