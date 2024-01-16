module RE2DFA where

-- | direct construction of DFA from RE.
-- DFA for r has as states r and its iterated derivatives.
-- The derivative operation is the transition function.

-- | regular expressions over alphabet a
data Regexp a
     = Null
     | Epsilon
     | Atom a
     | Concat (Regexp a) (Regexp a)
     | Union (Regexp a) (Regexp a)
     | Star (Regexp a)
     deriving (Eq, Show)

-- | example regular expressions
re1 = Epsilon
re15 = Concat (Atom "a") (Concat (Atom "b") (Atom "b"))
re2 = Concat (Concat (Concat (Star (Union (Atom "a") (Atom "b"))) (Atom "a")) (Atom "b")) (Atom "b")

-- | derivative of regular expression.
-- @derive r a@ computes @r'@ such that 
-- L(r') = { w | a w in L(r) }
-- cf. Brzozowski, Derivatives of Regular Expressions, JACM 11:4(481-494), 1964.
derive :: Eq a => Regexp a -> a -> Regexp a
derive (Null) a = Null
derive (Epsilon) a = Null
derive (Atom a') a = if a' == a then Epsilon else Null
derive (Union r1 r2) a = cUnion (derive r1 a) (derive r2 a)
derive (Concat r1 r2) a = if not (nullable r1) then
                          cConcat (derive r1 a) r2
                          else
                          cUnion (cConcat (derive r1 a) r2) (derive r2 a)
derive (Star r) a = cConcat (derive r a) (Star r)
-- Star r == Union Epsilon (Concat r (Star r))

-- | @nullable r@ returns @True@ if epsilon in L(r)
nullable :: Regexp a -> Bool
nullable Null = False
nullable Epsilon = True
nullable (Atom a) = False
nullable (Union r1 r2) = nullable r1 || nullable r2
nullable (Concat r1 r2) = nullable r1 && nullable r2
nullable (Star r) = True

-- | smart constructor for Union: applies algebraic law "Null is unit"
cUnion Null r = r
cUnion r Null = r
cUnion r1 r2 = Union r1 r2

-- | smart constructor for Concat: applies "Epsilon is unit" and "Null is null"
cConcat Epsilon r = r
cConcat r Epsilon = r
cConcat Null r = Null
cConcat r Null = Null
cConcat r1 r2 = Concat r1 r2
