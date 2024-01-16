type Parser a = String -> [(String, a)]



--------- Primitive Parser --------

success :: a -> Parser a
success wert string = [(string, wert)]


fail :: Parser a
fail _ = []


------- Kombinatoren ------------

char a (b:rest) = if a == b then [(rest, a)] else []
char _ _ = []



-- Auswahl-Kombinator
-- soll:
--  parser1 /|/ parser2 soll parser1 oder parser2 oder beide parsen lassen

-- x /|/ y

-- BNF: a ::= b | c
(/|/) :: Parser a -> Parser a -> Parser a
(parser1 /|/ parser2) string = parser1 string ++ parser2 string



-- BNF: a ::= b | <nichts>

optional :: Parser a -> a -> Parser a
optional p wert = p /|/ success wert

-- BNF: a ::= b c


-- Verkettungs-Kombinator
-- p1 /-/ p2 ist ein Parser, der zunächst p1 anwendet, dann p2 anwendet und beide Ergebnisse als Tupel zurückliefert.

(/-/) :: Parser a -> Parser b -> Parser (a, b)
(parser1 /-/ parser2) string = [ (rest2, (ret1, ret2)) | (rest1, ret1) <- parser1 string, (rest2, ret2) <- parser2 rest1 ]

-- BNF: vieleAs ::= a vieleAs
--      vieleAs ::= <nichts>
-- ein a könnte man ableiten durch
-- vieleAs => a vieleAs (Regel 1) => a <nichts> ==== a
-- Der 'many' Kombinator gibt eine Liste mit allen möglichen Ableitungen zurück.

many :: Parser a -> Parser [a]
many parser = viele /|/ keine
    where viele = modify tcons (parser /-/ many parser)
	  tcons (x, xs) = x:xs
	  keine = success []

-- rechts ignorieren 
-- x />-/ y parst ein x, das von einem y gefolgt sein muss.
-- Das y trägt aber zum Rückgabewert nicht bei.
(/>-/) :: Parser a -> Parser b -> Parser a
parser1 />-/ parser2 = modify fst (parser1 /-/ parser2)

-- links ignorieren
-- p1 /->/ p2 wendet p1 und dann p2 an, und liefert die Rückgabe von p2. Die Rückgabe von p1 wird verworfen. 
(/->/) :: Parser a -> Parser b -> Parser b
parser1 /->/ parser2 = modify snd (parser1 /-/ parser2)



modify :: (a -> b) -> Parser a -> Parser b
modify fun parser string = [ (rest, fun a) | (rest, a) <- parser string ]

---------------------- Beispiel

data SyntaxBaum = SyntaxBaum Ausdruck
                 deriving (Show)
data Ausdruck = Ausdruck :* Ausdruck
              | Ausdruck :+ Ausdruck
              | X
                deriving (Show)

start = modify SyntaxBaum ausdruck
{-
   Start -> Viele
   Viele -> Ausdruck
   Ausdruck -> Term
   Ausdruck -> Term "+" Ausdruck
   Term -> Faktor
   Term -> Faktor "*" Term
   Faktor -> "X"
   Faktor -> "(" Ausdruck ")"
-}

ausdruck :: Parser Ausdruck
ausdruck = term 
           /|/ 
           modify plusAusdruck (term  /-/ (char '+' /->/ ausdruck))
  where
  plusAusdruck (a1, a2) = a1 :+ a2

term :: Parser Ausdruck
term = faktor 
       /|/ 
       modify malAusdruck (faktor /-/ (char '*' /->/ term))
  where
  malAusdruck (a1, a2) = a1 :* a2

faktor :: Parser Ausdruck
faktor = modify (\_ -> X) (char 'X')
         /|/
         (char '(' /->/ (ausdruck />-/ char ')'))