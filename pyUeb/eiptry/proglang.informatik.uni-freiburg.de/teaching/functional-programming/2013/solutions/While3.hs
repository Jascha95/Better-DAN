module While3 where

import Data.Char
import Data.List
import Parser2
import Text.PrettyPrint hiding (empty)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.QuickCheck

-- | ----------------------- |
-- | The Program type
-- | ----------------------- |
data Program = Program [Stmt]
  deriving (Show, Eq)
data Stmt = While Exp [Stmt]
          | Asgn Id Exp
          | Print Exp
  deriving (Show, Eq)
data Exp = Cmp COp Exp Exp
         | If Exp Exp Exp
         | Not Exp
         | Num Integer
         | Op AOp Exp Exp
         | Var Id
  deriving (Show, Eq)
data COp = Eq | Neq | Le | Gt
  deriving (Show, Eq)
data AOp = Plus | Minus | Times | Div
  deriving (Show, Eq)
type Id = String

-- |--------------------------|
-- | An example program
-- |--------------------------|
ex_while1_str = "\n\
  \ x:=0; y :=5;\n\
  \ while x <= 10 do\n\
  \ y := (y * 5); x := (x + 1)\n\
  \ done;\n\
  \ y := if y > 10000 then 10000 else y fi\n\
  \ ;print (y + 5)\
  \ "

ex_while1 =
  Program
  [ Asgn "x" (Num 0)
  , Asgn "y" (Num 5)
  , While (Cmp Le (Var "x") (Num 10))
      [ Asgn "y" (Op Times (Var "y") (Num 5))
      , Asgn "x" (Op Plus (Var "x") (Num 1))
      ]
  , Asgn "y" (If (Cmp Gt (Var "y") (Num 10000)) (Num 10000) (Var "y"))
  , Print (Op Plus (Var "y") (Num 5))
  ]

-- |------------------------------------|
-- | An Arbitrary Instance for Programs |
-- |------------------------------------|
instance Arbitrary Program where
  arbitrary = Program <$> gen_stmts 10

gen_stmts n = resize n (listOf1 $ gen_stmt n)
gen_stmt 0 = Asgn <$> gen_id <*> gen_exp 3
gen_stmt n = frequency
              [ (1, While <$> gen_exp n <*> gen_stmts (n `div` 2))
              , (1, Asgn <$> gen_id <*> gen_exp n)
              , (1, Print <$> gen_exp n)
              ]

gen_exp 0 = Var <$> gen_id
gen_exp n = frequency 
        [ (2, gen_cmpExp )
        , (3, gen_aexp n)
        ]

  where nextN = n `div` 2
        expResize n' = resize (n' `div` 4)

        gen_cmpExp = expResize n $ oneof $
          [ Cmp <$> gen_cmp <*> gen_aexp nextN <*> gen_aexp nextN
          , If <$> gen_exp nextN <*> gen_exp nextN <*> gen_exp nextN
          , Not <$> gen_exp nextN
          ]
        

        gen_aexp n' = expResize n' $ oneof $
          [ (\(Positive i) -> Num i) <$> arbitrary
          , Op <$> gen_op <*> gen_aexp nextN <*> gen_aexp nextN
          , Var <$> gen_id
          ]


gen_op = elements [Plus, Minus, Times, Div]
gen_cmp = elements [Eq, Neq, Le, Gt]
gen_id = resize 10 $ ("var" ++) . show <$> choose (0, 20 :: Int)

-- |--------------------------|
-- | Pretty printing programs |
-- |--------------------------|

-- Use this function to print a program in human readable form (better
-- then show)
pp :: Program -> String
pp (Program ss) = render $ pp_stmts ss


-- You don't need to worry about the implementation (but you can, if you like)
pp_stmts ss = vcat $ punctuate (semi <+> space) $ map pp_stmt ss
pp_stmt (While e1 ss) =
  text "while" <+> pp_exp e1 <+> text "do"
  $$ wnest (pp_stmts ss)
  $$ text "done"
pp_stmt (Asgn i e) = text i <+> text ":=" <+> pp_exp e
pp_stmt (Print e) = text "print" <+> pp_exp e
pp_exp (Cmp op e1 e2) = pp_exp e1 <+> pp_cmp op <+> pp_exp e2
pp_exp (Not e) = text "not" <+> pp_exp e
pp_exp (Num i) = integer i
pp_exp (Op op e1 e2) = char '(' <> pp_exp e1 <+> pp_op op <+> pp_exp e2 <> char ')'
pp_exp (Var i) = text i
pp_exp (If e e1 e2) =
  text "if" <+> pp_exp e <+> text "then" <+> pp_exp e1 <+> text "else" <+> pp_exp e2 <+> text "fi"
pp_cmp = text . hshow_cmp
pp_op = text . hshow_op

wnest = nest 2

-- |--------------------------|
-- | The type of tokens
-- |--------------------------|
-- Use this lexer to tokenize the input before parsing
lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

-- These Tokens are produced by the lexer `lex'
data Token = TWhile
           | TPrint
           | TSep
           | TDo
           | TDone
           | TNot
           | TIf
           | TFi
           | TThen
           | TElse
           | TOpen
           | TClose
           | TCmp COp 
           | TOp AOp 
           | TAsgn
           | TNum Integer
           | TId Id
  deriving (Eq, Show)

hshow_token t =
  case t of
    TPrint  -> "print"
    TWhile  -> "while"
    TSep    -> ";"
    TDo     -> "do"
    TDone   -> "done"
    TNot    -> "not"
    TIf     -> "if"
    TFi     -> "fi"
    TThen   -> "then"
    TElse   -> "else"
    TOpen   -> "("
    TClose  -> ")"
    TAsgn   -> ":="
    TCmp op -> hshow_cmp op
    TOp  op -> hshow_op op
    TNum i  -> show i
    TId i   -> i

hshow_cmp Eq = "=="
hshow_cmp Le = "<="
hshow_cmp Gt = ">"
hshow_cmp Neq = "!="
hshow_op o =
  case o of
   Plus  -> "+"
   Minus -> "-"
   Times -> "*"
   Div   -> "/"


skipSpace = many (satisfy isSpace) 
p_tok = t_alnum
      <|> t_paren
      <|> t_op
      <|> t_cmp
      <|> t_sep
      <|> t_asgn
      <|> t_num

t_alnum = fmap mkToken $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  where mkToken "while"  = TWhile
        mkToken "print"  = TPrint
        mkToken "do"     = TDo
        mkToken "done"   = TDone
        mkToken "not"    = TNot
        mkToken "if"     = TIf
        mkToken "fi"     = TFi
        mkToken "then"   = TThen
        mkToken "else"   = TElse
        mkToken i        = TId i

t_paren = TOpen <$ lit '(' <|> TClose <$ lit ')'
t_cmp :: Parser Char Token
t_cmp = fmap TCmp $
      Le <$ string "<="
     <|> Gt <$ string ">"
     <|> Eq <$ string "=="
     <|> Neq <$ string "!="
t_op = fmap TOp $
     Plus <$ string "+"
     <|> Minus <$ string "-"
     <|> Times <$ string "*"
     <|> Div <$ string "/"
         
t_num = TNum . read <$> many1 (satisfy isDigit)
t_sep = TSep <$ lit ';'
t_asgn = TAsgn <$ string ":="

-- |------------------|
-- | Parser Utilities |
-- |------------------|
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs 
many1 p = (:) <$> p <*> many p


{-
 stmts ::= stmt; stmts
        |  eps
 stmt  ::= 'while' exp 'do' stmts 'done'
        |  id ':=' exp
 exp   ::= aexp cmp aexp
        | 'if' exp 'then' exp 'else' exp 'fi'
        |  'not' exp
        |  aexp
 aexp  ::= num
        |  id
        |  '(' aexp op aexp ')'
 cmp   ::= '<=' | '>' | '=' | '/='
 op    ::= '+' | '-' | '*' | '/'
 num   ::= "[0-9]+"
 id    ::= "[a-zA-Z][a-zA-Z0-9]*"
-}


parseProgram :: String -> Maybe Program
parseProgram = return . Program <=< parse p_stmts <=< lexer
         
p_stmts :: Parser Token [Stmt]
p_stmts =  (:) <$> p_stmt <*> many (lit TSep *> p_stmt ) 

p_stmt :: Parser Token Stmt
p_stmt = onId Asgn <*> (lit TAsgn *> p_exp)
      <|> While <$> (lit TWhile *> p_exp)
                <*> (lit TDo *> p_stmts <* lit TDone)
      <|> Print <$> (lit TPrint *> p_exp)


p_exp,p_aexp :: Parser Token Exp
p_exp = appInfix Cmp (onCmp id) p_aexp p_aexp
     <|> If <$> (lit TIf *> p_exp) 
            <*> (lit TThen *> p_exp)
            <*> (lit TElse *> p_exp)
            <* lit TFi
     <|> Not <$> (lit TNot *> p_exp)
     <|> p_aexp

p_aexp = onNum Num
      <|> lit TOpen *> appInfix Op (onOp id) p_aexp p_aexp <* lit TClose
      <|> onId Var


runTests = defaultMain
  [ testProperty "parsewhile" $ \p -> parseProgram (pp p) == Just p
  ]

-- helpers 

-- Some functions to extract further information contained in some
-- tokens. The code is quite repetitive; we will learn to avoid this
-- kind of repetition later in the course
onId :: (Id -> a) -> Parser Token a
onId f = do
  t <- p_anyTok
  case t of
    TId n -> return $ f n
    _     -> empty

onNum f = do
  t <- p_anyTok
  case t of
    TNum i -> return $ f i
    _      -> empty
onCmp f = do 
  t <- p_anyTok
  case t of
    TCmp o -> return $ f o
    _       -> empty

onOp f = do
   t <- p_anyTok
   case t of
     TOp o -> return $ f o
     _     -> empty

appInfix f po p p' = (\ x o x' -> f o x x') <$> p <*> po <*> p'


p_anyTok = satisfy (const True)


ex_while2 = 
 [ While
     (Op
        Plus
        (Op
           Div
           (Op Times (Op Minus (Num 1) (Var "var0")) (Var "var18"))
           (Op Minus (Op Div (Var "var4") (Var "var7")) (Var "var4")))
        (Var "var1"))
     [ While
         (Cmp
            Gt
            (Cmp
               Gt
               (Cmp Le (Var "var15") (Var "var11"))
               (Op
                  Plus
                  (Var "var1")
                  (Op
                     Plus
                     (Num 1)
                     (Op Minus (Var "var17") (Op Times (Var "var4") (Num 1))))))
            (Num 1))
         [ Asgn "var8" (Var "var18")
         ]
     ]
 ]

          
ex_whileBig = 
 [ While
     (Op
        Plus
        (Op
           Div
           (Op Times (Op Minus (Num 1) (Var "var0")) (Var "var18"))
           (Op Minus (Op Div (Var "var4") (Var "var7")) (Var "var4")))
        (Var "var1"))
     [ While
         (Cmp
            Gt
            (Cmp
               Gt
               (Cmp Le (Var "var15") (Var "var11"))
               (Op
                  Plus
                  (Var "var1")
                  (Op
                     Plus
                     (Num 1)
                     (Op Minus (Var "var17") (Op Times (Var "var4") (Num 1))))))
            (Num 1))
         [ Asgn "var8" (Var "var18")
         , While
             (If
                (Op Minus (Num 1) (Num 1))
                (Op Plus (Num 1) (Op Times (Num 1) (Var "var13")))
                (Op Minus (Var "var20") (Op Times (Var "var18") (Var "var7"))))
             [ While
                 (Cmp Le (Var "var10") (Var "var15"))
                 [ Asgn "var7" (Not (If (Var "var12") (Var "var14") (Var "var6"))) ]
             ]
         ]
     , While
         (Not (Cmp Neq (Not (Var "var20")) (Var "var7")))
         [ While
             (Num 1) [ Asgn "var16" (Cmp Le (Var "var6") (Var "var13")) ]
         , While (Var "var11") [ Asgn "var10" (Num 1) ]
         ]
     ]
 ]
