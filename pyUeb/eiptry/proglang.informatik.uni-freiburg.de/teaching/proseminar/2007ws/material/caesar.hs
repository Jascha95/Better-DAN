import Char

-- Kodierung

char2int :: Char -> Int
char2int c = ord c - ord 'a'


int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

-- shift :: Int -> Char -> Char

-- encode :: Int -> String -> String


-- Häufigkeitstabllen

table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
         6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]


-- freqs :: String -> [Float]

-- Chi-Square-Statistik

-- chisqr :: [Float] -> [Float] -> Float


--Cracking the code
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [n | (x',n) <- zip xs [0..length xs], x == x']

--crack :: String -> String
--crack xs = encode (-factor) xs
--  where 
--    factor = head (positions (minimum chitab) chitab)
--    chitab = [chisqr (rotate n table') table | n <- [0..25]]
--    table' = freqs xs
