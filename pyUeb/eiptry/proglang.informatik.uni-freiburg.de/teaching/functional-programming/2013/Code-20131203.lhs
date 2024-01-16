> module Week6 where
> import Data.Char
> import Data.List hiding (transpose)
> import Control.Monad
> import Control.Applicative
> import Test.QuickCheck

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

pempty recognizes the empty language

> pempty :: Parser t r
> pempty ts = []

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




* 20131128: An application to parsing expressions

Consider the grammar

E -> E "+" T
E -> T

E -> T E'
E' -> 
E' -> "+" T E'
T -> "let" Ident "=" E "in" E
T -> Ident
T -> Number
T -> "(" E ")"

x + 5 - z + 3
((x + 5) - z) + 3

parseE = palt (pseq parseE ...) parseT
defines a parser that never terminates on any input

Wanted: parseT "let x=5 in x+x"

A datatype to represent the result of parsing

> data E = V String | N Integer | A E E | L String E E
>   deriving (Eq, Show)

L :: String -> E -> E -> E

Transform the grammar; eliminate left recursion

The start of, we need a lexer that partitions the incoming list of
characters into a list of tokens. A token is either a single symbol,
an identifier, or a number. Whitespace characters are removed.

> data Token = Symbol Char | Ident String | Number Integer
>   deriving (Eq, Show)

> lexer :: String -> [Token]
> lexer [] = []
> lexer (c:cs) 
>   | isSpace c = lexer cs
>   | isAlpha c = Ident (c:acs) : lexer acs_rest
>   | isDigit c = Number (read $ c:ncs) : lexer ncs_rest
>   | otherwise = Symbol c : lexer cs
>   where acs = takeWhile isAlphaNum cs
>         acs_rest = dropWhile isAlphaNum cs
>         ncs = takeWhile isDigit cs
>         ncs_rest = dropWhile isDigit cs

> numberToken n (Number m:_) = m == n
> numberToken n _ = False

> prop_lex_num n = n >= 0 ==> numberToken n $ lexer (show n)

> isNumberToken (Number _) = True
> isNumberToken _ = False
> isIdentToken (Ident _) = True
> isIdentToken _ = False

> parserE :: Parser Token E
> parserE' :: Parser Token [E]
> parserT :: Parser Token E

> pIdent :: Parser Token String
> pIdent = pmap (\(Ident x) -> x) $ satisfy isIdentToken

> parserE  = pseq (pmap build_term parserT) parserE'
> parserE' = palt (pseq (pmap (const id) (lit $ Symbol '+'))
>                       (pseq (pmap (:) parserT) parserE')) (succeed [])
> parserT  = palt (pseq (pseq (pseq (pmap (const L) $ lit $ Ident "let")
>                       pIdent)
>                       (pseq (pmap (const id) $ lit $ Symbol '=')
>                       parserE))
>                       (pseq (pmap (const id) $ lit $ Ident "in")
>                       parserE)) $
>            palt (pmap (\(Ident x) -> V x) $ satisfy isIdentToken) $
>            palt (pmap (\(Number n) -> N n) $ satisfy isNumberToken) $
>            pseq (pmap (const id) $ lit $ Symbol '(')
>                 (pseq (pmap const $ parserE) (lit $ Symbol ')'))

> build_term :: E -> [E] -> E
> build_term t [] = t
> build_term t (t1:ts) = build_term (A t t1) ts

* 20131203: Parsers have rich structure ...
- many concepts from category theory can be mapped to programming concepts
- parsing fits many of these concepts

* Parsing is a functor

instance Functor (Parser t) where
  fmap f m = undefined

fmap :: Functor f => (a -> b) -> (f a -> f b)
map               :: (a -> b) -> ([a] -> [b])

> mapMaybe          :: (a -> b) -> Maybe a -> Maybe b
> mapMaybe f Nothing = Nothing
> mapMaybe f (Just a) = Just (f a)

- functorial laws

** fmap id_a == id_f_a
** fmap (f . g) == fmap f . fmap g


- for parsing: fmap = pmap

> newtype Parser' token result =
>   Parser' { exParser' :: Parser token result } 

> -- exParser' (Parser' p) = p

> instance Functor (Parser' token) where
>   fmap f (Parser' p) = Parser' (pmap f p)




* Parsing is a monad

instance Monad (Parser t) where
  return x = undefined
  ma >>= f = undefined

- monad laws
** return x >>= f  ==  f x
** ma >>= return   ==  ma        --  lhs:  ma >>= \x -> return x
** (ma >>= \x -> mb) >>= f  == ma >>= \x -> (mb >>= f)    --  x not in free(f)

- list monad:
instance Monad [] where
  return x = [x]
  ma >>= f = concat $ map f ma

-- ma       :: [a]
-- f        :: a -> [b]
-- ma >>= f :: [b]

Monad laws for the list monad:
(1) return x >>= f  ==
    [x] >>= f ==
    concat $ map f [x] ==
    concat $ [f x] ==
    f x
(2) [x1, x2, ...] >>= return   ==
    concat $ map (\x -> [x]) [x1, x2, ...] ==
    concat $ [[x1], [x2], ...] ==
    [x1, x2, ...]
(3) ...

> instance Monad (Parser' token) where
>   return x = Parser' (succeed x)
>   (Parser' p) >>= f = Parser' $
>               \ts -> [(b, ts'') | (a, ts') <- p ts, (b, ts'') <- exParser' (f a) ts']

using the list monad:  do { (a, ts') <- p ts; exParser' (f a) ts' }

ma :: Parser' t a
f  :: a -> Parser' t b
.. :: Parser' t b

fulfills the monad laws ...

can use the do notation for parsers:
to parse
 T -> "let" Ident "=" E "in" E 

do _ <- lit $ Ident "let"
   x <- pIdent
   _ <- lit $ Symbol '='
   e1 <- parserE
   _ <- lit $ Ident "in"
   e2 <- parserE
   return $ L x e1 e2

... readability is much improved ...






* Parsing is a monad with summation

instance MonadPlus (Parser t) where
  mzero = undefined
  mplus m1 m2 = undefined

for lists:

instance MonadPlus [] where
  mzero = []
  mplus = (++)

> instance MonadPlus (Parser' t) where
>   mzero = Parser' pempty
>   mplus (Parser' p1) (Parser' p2) = Parser' (palt p1 p2)













* Parsing is applicative

** Origin: applicative programming with effects (McBride, Paterson)

- example 1 : sequencing computation

sequence :: [IO a] -> IO [a]
sequence []       = return []
sequence (io:ios) = do x <- io
                       xs <- sequence ios
                       return (x:xs)

sequence (io:ios) = liftM2 (:) io (sequence ios)

(:) :: a -> [a] -> [a]
|
V
liftM2 (:) :: IO a -> IO [a] -> IO [a]

sequence []       = return []
sequence (io:ios) = return (:) `ap` io `ap` sequence ios

return (:)                  :: IO (a -> [a] -> [a])
return (:) `ap` io          :: IO ([a] -> [a])
return (:) `ap` io `ap` ios :: IO [a]

return :: Monad m => a -> m a
ap     :: Monad m => m (a -> b) -> m a -> m b

ma >>= \x -> if x then mb1 else mb2 





- example 2 : list of list transposition

> transpose :: [[a]] -> [[a]]
> transpose [] = repeat []
> transpose (xs:xss) = zipWith (:) xs (transpose xss)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

repeat :: a -> [a]

> zapp :: [a -> b] -> [a] -> [b]
> zapp fs xs = zipWith ($) fs xs

zipWith_n :: (a1 -> a2 -> ... -> an -> b) -> [a1] -> [a2] -> ... -> [an] -> [b]
zipWith_n f xs1 xs2 ... xsn = repeat f `zapp` xs1 `zapp` xs2 ...

transpose []       = repeat []
transpose (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss












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
  pure  :: a -> f a
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
fa *> fb = fmap (const id) fa <*> fb

(<*) :: f a -> f b -> f a
fa <* fb = fmap const fa <*> fb

> instance Applicative (Parser' token) where
>   pure = return
>   (<*>) = ap

> instance Alternative (Parser' token) where
>   empty = mzero
>   (<|>) = mplus

can use the applicative notation for parsers:
to parse
   T -> "let" Ident "=" E "in" E 
   T -> Ident
   T -> Number
   T -> '(' E ')'

parserT = 
   pure L <* lIdent "let" <*> pIdent <* lSymbol '=' <*> parserE <* lIdent "in" <*> parserE
   <|> pure Ident <*> satisfy isIdentToken
   <|> pure Number <*> satisfy isNumberToken
   <|> lSymbol '(' *> parserE <* lSymbol ')'

