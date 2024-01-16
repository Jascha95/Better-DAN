import Char

-- Kodierung

char2int :: Char -> Int
char2int c = ord c - ord 'a'


int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c = int2char ((char2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n [] = []
encode n (x:xs) 
         | isLower x = (shift n x):(encode n xs)
         | otherwise = x:(encode n xs)

-- Häufigkeitstabllen

table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
         6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

rel_freq :: [Int] -> [Float]
rel_freq xs = [(fromIntegral x) / n | x <- xs]
                       where  n = fromIntegral (sum xs)

abs_freq :: String -> [Int]
abs_freq xs = [length ( positions (int2char x) xs) | x <- [0..25]]

freqs :: String -> [Float]
freqs xs = rel_freq (abs_freq xs)

-- Chi-Square-Statistik

chisqr :: [Float] -> [Float] -> Float
chisqr xs ys = sum [(y-x)^2 / y | (x,y) <- zip xs ys]

--Cracking the code
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [n | (x',n) <- zip xs [0..length xs], x == x']

crack :: String -> String
crack xs = encode (-factor) xs
 where 
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
