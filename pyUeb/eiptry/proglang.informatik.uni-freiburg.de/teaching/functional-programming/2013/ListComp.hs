import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
-- | ---------------------------------- |
-- | Desugaring List Comprehensions     |
-- | ---------------------------------- |
--
-- List comprehensions are just abbreviations for regular Haskell
-- definitions (i.e. they are ``syntactic sugar''). In the following
-- we will see how the Haskell Compiler systematically translates a
-- list comprehension into a Haskell expression.

------------------
-- Prelude
------------------
-- We will use let-expressions in order to write down the
-- desugaring. In case you do not know them, here is a short
-- introduction:
--
-- Let expressions allow to make local definitions similary to
-- where-clauses:
v = let xs = [1..3]
        ys = [5..6]
        f x = x * 8
    in sum (map f (xs ++ ys)) -- ---> 136

-- They have the form :
-- let decls
-- in e 
--
-- where `e' is a Haskell expression and `decls' are a ``block'' of definitions, as
-- would be allowed in a where-clause.

-- The main difference between let-expressions and where-clauses is
-- that the latter can only be defined directly under a definition,
-- while the former can be used everywhere an expression is allowed:
v' = let xs = [1..3]
         ys = [5..6]
         f x = x * (let g x = x + head xs in g 8)
      -- f x = x * (g 8 where g x = x + head xs) <-- syntax error
     in sum (map f (xs ++ ys)) -- ---> 153

--------------------------------------
-- ``Decomprehension'': General Scheme
--------------------------------------
-- List comprehensions have the following syntactic structure:
--   [e | qs ]
-- where
--   - e is a Haskell expression
--   - qs is a comma seperated list of qualifiers q
--   - q is either 
--     - p <- e  (a generator)
--     - e  (a guard)
--     - let decls (a local definition)
--   - p is a pattern
--   - decls are declarations that are allowed in a let-expression. 

-- The compiler now applies the algorithm ``decomp'' that translates a
-- list comprehension into a haskell expression without list
-- comprehensions. Here it is, described with ``transformation
-- rules'' of the form [e | qs] --decomp--> e. They transform a list
-- comprehension into a Haskell expression. The algorithm uses those
-- rules repeatedly on all list-comprehensions in a haskell program,
-- until there are no more list-comprehensions left. 

-- If there are no more qualifiers in a list-comprehension, it is
-- replaced by a single-element list containing the expression e

-- [e | ] --decomp--> [e]


-- if the next qualifier of a list comprehension is a generator:
-- [e | p <- e', qs] --decomp-->  let f p = [e, qs]
--                                    f _ = []
--                                in concat (map f e')
-- note that if the pattern p is not matched, the empty list is
-- returned.

-- if the next qualifier is a guard:
-- [e | e',qs] --decomp--> if e' then [e, qs] else []
-- note that if the guard evaluates to False, the empty list is returned

-- if the next qualifier is a local definition:
-- [e | let decls, qs] --decomp--> let decls in [e, qs]

------------------
-- Examples:
------------------
-- This function simply transforms an Int list into a list of tuples
-- where the second component is zero
set0 xs = [(x, 0) | x <- xs ]
-- after decomp
set0' xs = let f x = [(x, 0)] -- never fails
               f _ = []
           in concat (map f xs)

prop_set0_set0' :: [Int] -> Bool
prop_set0_set0' xs = set0 xs == set0' xs

-- All combinations of odd elements from xs with even elements from ys
oddEvens xs ys = [ (x, y) | x <- xs, odd x, y <- ys,  even y ]
-- after 2 applications of decomp 
oddEvens' xs ys = let f x = if odd x
                            then [(x,y) | y <- ys, even y]
                            else []
                      f _ = [] -- never fails
                  in concat (map f xs)

-- all applications of decomp 
oddEvens'' xs ys = let f x = if odd x
                             then let f y = if even y
                                            then [(x, y)]
                                            else []
                                  in concat (map f ys)
                             else []
                       f _ = [] -- never fails
                   in concat (map f xs)

prop_oddEvens_oddEvens'_oddEvens'' :: [Int] -> [Int] -> Bool
prop_oddEvens_oddEvens'_oddEvens'' xs ys =
  oddEvens xs ys == oddEvens' xs ys && oddEvens xs ys == oddEvens'' xs ys

-----------------------
-- Video Example
-----------------------
-- Now you can try to understand, how the Video-Store example from the
-- lecture can be rewritten without list comprehensions.

type Name = String
type Title = String
type Year = Int
type Age = Int

type User = (Name, Year)
--        ^ name   ^ Year of birth 
type Film = (Title, Age)
--                   ^ fsk           
type Purchase = (Name, Title, Year) -- <---+
--             ^ user name ^ item name     ^ date of purchase

users :: [User]
users = [ ("Klein Anton", 2000)
        , ("Alter Verwalter", 1950)
        ]
purchases :: [Purchase]
purchases =
  [ ("Alter Verwalter", "Die Körperfresser kommen", 1978)
  , ("Klein Anton", "Texas Chainsaw Massacre", 2013)
  , ("Klein Anton", "Bambi", 2013)
  , ("Alter Verwalter", "Bambi", 1954)
  ]
                                                         
items :: [Film] 
--         
items =
  [ ("Die Körperfresser kommen", 18)
  , ("Bambi", 6)
  , ("Texas Chainsaw Massacre", 18) ]


fskProblems :: [(String, String, Int, Int)]
--               ^ buyer  ^ title ^ age ^fsk              
fskProblems =
  [(buyer, title, age, fsk) |
     (uName, bYear) <- users,
     (buyer, title, pYear) <- purchases,
     buyer == uName,
     (iName, fsk) <- items,
     let age = pYear - bYear,
     title == iName &&
      age < fsk ]


fskProblems' = let f (uName, bYear) =
                     let f (buyer, title, pYear) =
                           if buyer == uName
                           then let f (iName, fsk) =
                                      let age = pYear - bYear
                                      in if title == iName && age < fsk
                                         then [(buyer, title, age, fsk)]
                                         else []
                                    f _ = []
                                in concat (map f items)
                           else []
                         f _  = []
                     in concat (map f purchases)
                   f _ = [] 
               in concat (map f users)


test_fskProblems_fskProblems' = fskProblems == fskProblems'

                                

tests =
  [ testProperty "set0" prop_set0_set0'
  , testProperty "oddEvens" prop_oddEvens_oddEvens'_oddEvens''
  , testProperty "fskProblems" test_fskProblems_fskProblems'
  ]
