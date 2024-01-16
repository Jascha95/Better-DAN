import Test.QuickCheck

-- example tuples
examplePair :: (Double, Bool)
examplePair = (3.14, False)

exampleTriple :: (Bool, Int, String)
exampleTriple = (False, 42, "Answer")

exampleFunction :: (Bool, Int, String) -> Bool
exampleFunction (b, i, s) = not b && length s < i

-- function over lists - examples
summerize :: [String] -> String
summerize []  = "None"
summerize (x : []) = "Only " ++ x
summerize [_,_] = "Two things: ???"
summerize _  = "Several things."


-- doubles [3,6,12] = [6,12,24]
doubles :: [Integer] -> [Integer]
doubles []     = []
doubles (x:xs) = (2 * x : doubles xs)

-- mymap f [x1, x2, ..., xn] = [f x1, f x2, ..., fn]
mymap f []     = []
mymap f (x:xs) = (f x : mymap f xs)

-- twice the argument
double :: Integer -> Integer
double x = 2 * x

-- doubling the elements of the list, 2nd try
doubles' xs = mymap double xs

-- filter even [1..9] gives [2,4,6,8]
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p []     = []
myfilter p (x:xs) | p x = x : myfilter p xs
          | otherwise = myfilter p xs


-- check that the two implementations of doubles are equivalent
prop_doubles_is_doubles xs = doubles xs == doubles' xs

