-- crash course Haskell programming
-- Peter Thiemann
-- 20040430

-- setup lang/ghc6.2.1
-- setup lang/gcc3
-- setup internet

-- Program.hs
-- Compiler: washc
-- Benutzung: washc Program
--            liefert ausführbares Programm a.out
-- Alternativ: washc Program -o Program.cgi
--            liefert ausführbares Program Program.cgi

module CrashKurs where

-- Hauptprogramm: module Main where ...

-- Liste von Gleichungen

x :: Int  -- Maschineninteger
x = 5

d :: Double
d = 3.5

c :: Char
c = 'x'

b :: Bool
b = True -- False

s :: String
s = "Ein schöner String"

y = 3*x+2

------- Funktionsdefinitionen

f :: Int -> Int
f x = x*x

fx = f 5

g :: Int -> Int -> Int
g x y = 3*x+y

gxy = g 5 7

-- partielle Anwendung von g
g5 = g 5

gxy2 = g5 7

-------------------------------------------------
-- Funktionen auf Listen
-- Liste ist eingebauter Datentyp
-- !alle Elemente haben den gleichen Typ

l0 :: [a]
l0 = []

l1 :: [Int]
l1 = [1,2,3]

l2 = 1 : l0
l3 = l1 : l0
{- 
*CrashKurs> l3
[[1,2,3]]
cmTypeOfName: it
it :: [[Int]]
-}

l4 = 4 : 5 : l1

{- 3 mal Typfehler:
l4kaputt = (4 : 5) : l1
l4kaputt = (4 : l1) : l1
l4kaputt = l1 : l1
-}

-- Listenverkettung
l5 = l1 ++ [4, 5]

-- Funktionen über Listen

-- zwei Fälle: leere Liste oder kopf:rest
-- verwende Konstruktoren [] und : auf der linken Seite

h :: [Int] -> [Int]
h [] = []
h (x:xs) = 4*x : h xs

-- wichtiger Sonderfall: type String = [Char]

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Eingabe und Ausgabe

-- beschrieben durch abstrakten Datentyp
-- IO a
-- Werte dieses Typs sind
-- Ein/Ausgabeaktionen, die Werte vom Typ a berechnen

io1 = putStrLn "erste Ausgabe"
io2 = getChar

puts :: [Char] -> IO ()
puts [] = return ()
puts (x:xs) = do putChar x
		 puts xs

puts' :: [Char] -> IO ()
puts' [] = return ()
puts' (x:xs) = do { putChar x; puts xs;}

-- | die IO Aktion, die nichts tut
-- return :: a -> IO a

