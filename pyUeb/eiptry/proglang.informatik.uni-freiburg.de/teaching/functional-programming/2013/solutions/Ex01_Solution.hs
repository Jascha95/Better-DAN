module Ex01_Solution where
       
import Data.Char
import Data.List
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


-- |---------------|
-- | Aufgabe 1
-- |---------------|

maxi :: Integer -> Integer -> Integer
maxi n m | n <= m = m
         | otherwise = n
     
mini :: Integer -> Integer -> Integer
mini n m | n <= m = n     
         | otherwise = m     
     
maxi3 :: Integer -> Integer -> Integer -> Integer
maxi3 n1 n2 n3 = maxi (maxi n1 n2) n3      

------
-- ,,Offensichtlich richtige'' Version
med :: Integer -> Integer -> Integer -> Integer
med n1 n2 n3 | isMedian n1 n2 n3 = n2 
             | isMedian n1 n3 n2 = n3
             | isMedian n3 n1 n2 = n1
  where isMedian x y z = (x <= y && y <= z) || (z <= y && y <= x)


------
-- Alternative
med' :: Integer -> Integer -> Integer -> Integer
med' n1 n2 n3 = maxi3 (mini n1 n2) (mini n2 n3) (mini n1 n3)

---------------
-- Properties
minMaxTest = testGroup "min & max & med"
             [ testProperty "maxi and mini are opposite" prop_maxiOppositeMini
             , testProperty "maxi is last of sorted list" prop_maxiIsLastOfSorted
             , testProperty "mini is first of sorted list" prop_miniIsFirstOfSorted
             , testProperty "maxi 3 is last of ..." prop_maxi3IsLastOfSorted
             , testProperty "med is middle of ..." prop_medIsMiddleOfSorted
             , testProperty "med is med'" prop_medIsMed
             ]
        
prop_maxiOppositeMini x y       = not (x /= y) || (mini x y /= maxi x y)
prop_maxiIsLastOfSorted x y     = maxi x y == last (sort [x, y]) 
prop_miniIsFirstOfSorted x y     = mini x y == head (sort [x, y])
prop_maxi3IsLastOfSorted x y z  = maxi3 x y z == last (sort [x, y, z])
prop_medIsMiddleOfSorted x y z  = med x y z == head (drop 1 (sort [x, y, z]))
prop_medIsMed x y z = med x y z == med' x y z


-- |------------------------------------|       
-- | Aufgabe 2
-- |------------------------------------|       

pop :: [Int] -> [Int] 
pop s = drop 1 s
    
push :: Int -> [Int] -> [Int]
push i s = i:s  

------
-- look at the top element
peek :: [Int] -> Int
peek []     = 0
peek (x:xs) = x

dup :: [Int] -> [Int]
dup s = push (peek s) s
     
add, substract, multiply :: [Int] -> [Int]

------
-- a little messy... but independent of Stack internals (no mention of (:))
add s = push (peek s + peek (pop s)) (pop (pop s))
     
-- depends on the Stack being a list of IntS
multiply (s1:s2:s) = push (s1 * s2) s
multiply s         = push 0 (pop s)

neg :: [Int] -> [Int]
neg s = push (-1 * peek s) (pop s)
  
substract s = add (neg s)

noop :: [Int] -> [Int]
noop s = s  
  
-- Schreiben Sie einige QC Properties für die Kommandos
prop_popEverything xs = popN (length xs) xs == []
prop_popMore xs = popN (length xs + 8) xs == []
prop_popRemovesTopElement x xs = pop (x:xs) == xs
prop_pushAddsTop x xs = push x xs == (x:xs)
prop_pushPopInverse x xs = pop (push x xs) == xs
prop_dup x xs = dup (push x xs) == x:x:xs
prop_add x y s = (add (push x $ push y s)) == push (x + y) s
prop_add1 x = add (push x []) == [x]
prop_substr x y s = (substract (push x (push y s))) == push (y - x) s
prop_substr1 x = (substract (push x [])) == [-x]
prop_mult x y s = (multiply (push x $ push y s)) == push (x*y) s
prop_mult1 x = (multiply (push x [])) == [0]
prop_neg x xs = neg (push x xs) == (-x):xs
test_opEmpty = zeroOnTop (add [])
            && zeroOnTop (substract [])
            && zeroOnTop (dup [])
            && zeroOnTop (multiply [])
            && (zeroOnTop (neg []))

----
-- Helper for Properties
popN :: Int -> [Int] -> [Int]
popN n s | n <= 0 = s
         | otherwise = pop (popN (n-1) s)


zeroOnTop xs = peek xs == 0

stackTests = testGroup "stack tests"
             [
             testProperty "pop everything" 
                  (forAll (resize 1000 arbitrary) prop_popEverything)
             , testProperty "pop more" 
                  (forAll (resize 1000 arbitrary) prop_popMore)
             , testProperty "pop remove top" prop_popRemovesTopElement
             , testProperty "push add top" prop_pushAddsTop
             , testProperty "push and pop are inverse" prop_pushPopInverse
             , testProperty "dup" prop_dup
             , testProperty "add" prop_add
             , testProperty "add1" prop_add1
             , testProperty "substr" prop_substr
             , testProperty "substr1" prop_substr1
             , testProperty "mult" prop_mult
             , testProperty "mult1" prop_mult1
             , testProperty "neg" prop_neg
             , testProperty "empty ops" test_opEmpty
             ]


isInt :: String -> Bool   
isInt input | null input = True
            | otherwise = isNumber (head input) && isInt (drop 1 input)
 
readInt :: String -> Int
readInt input = readInt' (length input) input
readInt' i input | null input = 0
                 | otherwise = (ord (head input) - ord '0') * (10 ^ (i -1))
                               + readInt' (i - 1) (drop 1 input)

readCommand :: String -> [Int] -> [Int]
readCommand cmd s | cmd == "add" =  add s
                  | cmd == "substract" = substract s
                  | cmd == "pop" = pop s
                  | isInt cmd = push (readInt cmd) s
                  | otherwise = noop s



