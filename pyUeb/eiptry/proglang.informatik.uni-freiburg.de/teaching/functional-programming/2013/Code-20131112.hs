import Prelude hiding (lines, words)
import Data.Char (chr, isSpace)
import Test.QuickCheck
import Data.List hiding (lines, words)

data List a = Empty | Add a (List a)

alist = [("xxx", 1), ("zzz", 2)]


f_either :: Int -> Either Int Char
f_either n = if even n then Left (n `div` 2) else Right (chr n)


mytake, mydrop :: Int -> [a] -> [a]
mytake n xs | n <= 0 = []
mytake n [] = []
mytake n (x:xs) | n>0 = x : mytake (n-1) xs

mydrop n xs | n <= 0 = xs
mydrop n [] = []
mydrop n (x:xs) | n>0 = mydrop (n-1) xs

prop_take n xs = mytake n xs == take n xs
prop_drop n xs = mydrop n xs == drop n xs

prop_take_drop n xs = take n xs ++ drop n xs == xs


myzip :: [a] -> [b] -> [(a, b)]
myzip [] ys = []
myzip xs [] = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys

prop_zip xs ys = myzip xs ys == zip xs ys


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort smaller ++ [x] ++ qsort greater
      where smaller = [ y | y <- xs , y < x ]
            greater = [ y | y <- xs , y >= x ]

prop_qsort xs = qsort xs == sort xs

prop_qsort_1 xs = length (qsort xs) == length xs
prop_qsort_stable xs = qsort (qsort xs) == qsort xs

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x1:xs@(x2:_)) = x1 <= x2 && sorted xs
-- as-pattern

prop_qsort_sorted xs = sorted (qsort xs)


takeLine [] = []
takeLine (x:xs) | x /= '\n' = x : takeLine xs
                | otherwise = []

-- intermezzo:
{- the compiler would be able to recognize this non-terminating defintion
   x and y are both undefined
x = y+1
y = x+1
-}

lines :: String -> [String]
lines [] = []
lines s = takeWhile (/= '\n') s :
          (lines $ drop 1 $ dropWhile (/= '\n') s)

segments :: (a -> Bool) -> [a] -> [[a]]
segments p [] = []
segments p s = takeWhile p s : (segments p $ drop 1 $ dropWhile p s)

mylines = segments (/= '\n')

prop_Lines_Mylines xs = lines xs == mylines xs

-- commaSep "1,morning,8:45,42.3,5" == ["1", "morning", "8:45", "5"]
commaSep :: String -> [String]
commaSep = segments (/=',')

-- words: split a string into words
-- * words are separated by one or more white space characters
words :: String -> [String]
words = filter (not . null) . segments (not . isSpace)
-- (\c -> not (isSpace c))

mysum = foldr (+) 0

countWords' :: String -> String
countWords' = unlines .
              map (\(xs, n) -> xs ++ ": " ++ show n) .
              map (\xs -> (head xs, length xs)) .
              groupBy (==) .
              sort .
              words

countWords  = unlines .
              map (\xs -> (head xs) ++ ": " ++ show (length xs)) .
              groupBy (==) .
              sort .
              words
