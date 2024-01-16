module MiniWhile where

import Control.Monad.Trans.State
import Data.Char
import ParserCon
import qualified Data.Map.Strict as M
    
-- ^ Eval simple

type Env = M.Map Id Integer
type InterpM = State Env

execP s = env
    where 
      program = case parseString s of
                  Nothing -> error "Parsing failed"
                  Just x -> x
      ((), env) = runState (evalProgram program) M.empty
  

evalProgram (Program p) = evalStmts p

evalStmts = mapM_ evalStmt
                          
evalStmt :: Stmt -> InterpM ()
evalStmt (Asgn id e) = do
  v <- evalExp e
  assign id v
evalStmt s@(While b p) = do
  b <- evalExp b
  if b /= 0 then evalStmts p >> evalStmt s
  else return ()
  
evalExp (Num i) = return i
evalExp (Var id) = lookupId id
evalExp (BinOp op e1 e2) = evalOp op <$> evalExp e1 <*> evalExp e2
evalExp (If cond ethen eelse) = do
  b <- evalExp cond
  if b /= 0 then evalExp ethen else evalExp eelse
evalExp (Not b) = boolToInt . (/=) 0 <$> evalExp b
     
assign :: Id -> Integer -> InterpM ()
assign id v = do
  env <- get
  put $ M.insert id v env
lookupId :: Id -> InterpM Integer
lookupId id = M.findWithDefault 0 id <$> get
            
evalOp Plus   = (+) 
evalOp Minus  = (-)
evalOp Times  = (*)
evalOp Div    = div
evalOp Eq = \x y -> boolToInt $ x == y
evalOp Le = \x y -> boolToInt $ x <= y

boolToInt True = 1
boolToInt False = 0

    
-- ^ Parsing

data Program = Program [Stmt]
  deriving (Show, Eq)
data Stmt = Asgn Id Exp
          | While Exp [Stmt]
            -- Add more things here
  deriving (Show, Eq)
data Exp = Num Integer
         | Var Id
         | BinOp Op Exp Exp
         | If Exp Exp Exp
         | Not Exp
         -- Add more things here
  deriving (Show, Eq)
type Id = String
data Op = Plus | Minus | Times | Div | Eq | Le
          deriving (Show, Eq) 


parseString :: String -> Maybe Program
parseString s = do
  l <- lexer s
  parse parser l

parser :: Parser Token Program
parser = Program <$> p_stmts

p_stmts :: Parser Token [Stmt]
p_stmts =  (:) <$> p_stmt <*> many (lit TSep *> p_stmt)

p_stmt :: Parser Token Stmt
p_stmt = onId Asgn <*> (lit TAsgn *> p_exp)
      <|> While <$> (lit TWhile *> p_exp)
                <*> (lit TDo *> p_stmts <* lit TDone)

p_exp :: Parser Token Exp
p_exp = p_binOp p_aexp p_aexp
     <|> If <$> (lit TIf *> p_exp) 
            <*> (lit TThen *> p_exp)
            <*> (lit TElse *> p_exp)
            <* lit TFi
     <|> Not <$> (lit TNot *> p_exp)
     <|> p_aexp
p_aexp = onNum Num
      <|> lit TOpen *> p_binOp p_aexp p_aexp <* lit TClose
      <|> onId Var
p_binOp p p' = (\ x o x' -> BinOp o x x') <$> p <*> (onOp id) <*> p'

on :: (a -> Maybe b) -> (b -> c) -> Parser a c
on test f = do
   t <- satisfy (const True)
   case test t of
     Nothing -> empty
     Just x -> return $ f x

onId = on $ \s -> case s of {TId x -> Just x ; _ -> Nothing}
onNum = on $ \s -> case s of {TNum n -> Just n ; _ -> Nothing}
onOp = on $ \s -> case s of {TOp o -> Just o ; _ -> Nothing}
     
-- ^ Lexing
-- Use this lexer to tokenize the input before parsing

data Token = TSep -- ';'
           | TAsgn -- ':='
           | TNum Integer
           | TOp Op
           | TId Id
           | TWhile | TDo | TDone
           | TIf | TThen | TElse | TFi | TNot
           | TOpen | TClose
           -- Add more things here
  deriving (Eq, Show)

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

skipSpace = many (satisfy isSpace)
p_tok =
  t_alnum
  <|> t_sep
  <|> t_asgn
  <|> t_num
  <|> t_op
  <|> t_paren

t_num = TNum . read <$> many1 (satisfy isDigit)
t_sep = TSep <$ lit ';'
t_asgn = TAsgn <$ string ":="
t_alnum = fmap mkToken $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  where mkToken "while"  = TWhile
        mkToken "do"     = TDo
        mkToken "done"   = TDone
        mkToken "not"    = TNot
        mkToken "if"     = TIf
        mkToken "fi"     = TFi
        mkToken "then"   = TThen
        mkToken "else"   = TElse
        mkToken i        = TId i

t_paren = TOpen <$ lit '(' <|> TClose <$ lit ')'
t_op = fmap TOp $
     Plus <$ string "+"
     <|> Minus <$ string "-"
     <|> Times <$ string "*"
     <|> Div <$ string "/"
     <|> Le <$ string "<="
     <|> Eq <$ string "=="

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs 
many1 p = (:) <$> p <*> many p
