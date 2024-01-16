import Data.Char
import Test.QuickCheck
import Data.List

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

















segments p [] = []
segments p xs = takeWhile p xs : segments p (drop 1 (dropWhile p xs))
