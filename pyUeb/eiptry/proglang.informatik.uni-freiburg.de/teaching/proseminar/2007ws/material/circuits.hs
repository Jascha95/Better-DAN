import Control.Arrow
import List


-- Visualization class
class Signal a where
	showSignal :: [a] -> String

-- Implementation for Boolean signals
instance Signal Bool where
	showSignal bs = concat top++"\n"++concat bot++"\n"
		where (top,bot)      = unzip(zipWith sh (False:bs) bs)
		      sh True True   = ("__","  ")
		      sh False True  = (" _","| ")
		      sh True False  = ("  ","|_")
		      sh False False = ("  ","__")

-- Implementation for signals of pairs of Booleans
instance (Signal a, Signal b) => Signal (a,b) where
	showSignal xys = showSignal (map fst xys) ++ showSignal(map snd xys)

-- Implementation for signals of triples of Booleans
sta (x,y,z) = x
mid (x,y,z) = y
lst (x,y,z) = z
instance (Signal a, Signal b, Signal c) => Signal (a,b,c) where
	showSignal xyzs = showSignal (map sta xyzs) ++ showSignal(map mid xyzs) ++ showSignal (map lst xyzs)

-- Implementation for a list of signals
instance Signal a => Signal [a] where
	showSignal = concat . map showSignal . transpose

-- Logic functions with one argument
idA :: Arrow a => a Bool Bool
idA =  arr (\x -> x)

notA :: Arrow a => a Bool Bool
notA =  arr (\x -> not x)

-- Logic functions with two arguments
andA :: Arrow a => a (Bool,Bool) Bool
andA =  arr (\(x,y) -> x && y)

orA :: Arrow a => a (Bool,Bool) Bool
orA =  arr (\(x,y) -> x || y)

xorA :: Arrow a => a (Bool,Bool) Bool
xorA =  arr (\(x,y) -> not (x == y))

-- Composed functions
nandA :: Arrow a => a (Bool,Bool) Bool
nandA =  andA >>> notA

norA :: Arrow a => a (Bool,Bool) Bool
norA =  orA >>> notA

-- Signal maker
sig :: [(Int,a)] -> [a]
sig = concat . map (uncurry replicate)

-- Show input and output
-- USAGE: putStr$ mergeIOR f as
sim :: (Signal a, Signal b) => (b -> a) -> [b] -> String
sim f as =  (showSignal as) ++ (showSignal (map f as))

-- All possible inputs for a logical function with 2 arguments
-- For better readability, every combination is evaluated twice
twoInputs :: [(Bool,Bool)]
twoInputs =  sig [(2,(False,False)),(2,(False,True)),(2,(True,False)),(2,(True,True))]
-- All possible inputs for a logical function with 3 arguments
threeInputs :: [(Bool,Bool,Bool)]
threeInputs =  sig [(2,(False,False,False)),(2,(False,False,True)),
	(2,(False,True,False)),(2,(False,True,True)),(2,(True,False,False)),
	(2,(True,False,True)),(2,(True,True,False)),(2,(True,True,True))]


-- EXERCISES:
-- For testing, the sim function from above can be used
-- 
-- Example: putStr$ sim halfAdd twoInputs 

-- Half adder
--halfAdd :: Arrow a => a (Bool,Bool) (Bool,Bool)

-- Full adder
--fullAdd :: Arrow a => a (Bool,Bool,Bool) (Bool,Bool)


