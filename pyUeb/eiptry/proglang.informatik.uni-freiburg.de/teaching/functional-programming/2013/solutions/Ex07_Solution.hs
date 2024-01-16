module Ex07_Solution where


import Parser2
import While

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

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
