module Main where

import Data.Char

-- ^ Stack Calculator

type Stack = [Int]

type StackOp = Stack -> Stack
add, substract, multiply, neg, dup, pop, noop :: StackOp

push :: Int -> StackOp
push x xs = x:xs

pop (_:xs) = xs
pop [] = []

dup (x:xs) = x:x:xs
dup [] = []

binop f (x:y:xs) = f x y : xs
binop f [x] = [ f x 0 ]
binop f [] = [f 0 0]

neg s = push (-1 * peek s) (pop s)

add = binop (+)
substract = binop (-)
multiply = binop (*)

noop s = s

peek (x:xs) = x
peek [] = 0


-- ^ CLI

isInt :: String -> Bool
isInt s = s /= "" && all isDigit s

readCommand :: String -> [Int] -> [Int]
readCommand cmd s | cmd == "add" || cmd == "+" =  add s
                  | cmd == "substract" || cmd == "-"= substract s
                  | cmd == "multiply" || cmd=="*" =  multiply s
                  | cmd == "pop" = pop s
                  | cmd == "dup" = dup s
                  | isInt cmd = push (read cmd) s
                  | otherwise = noop s

main :: IO ()
main = runCalc []

-- not the use of `maybe' (if not yet known)
-- Defined in Prelude:
-- maybe :: b -> (a -> b) -> Maybe a -> b
runCalc st = do
  printStack st >> getLine >>= interpret st >>= maybe (return ()) runCalc

interpret st cmd | validCmd cmd = return $ Just $ readCommand cmd st
interpret st cmd | cmd == "exit" = do
  putStrLn "Good Bye!"
  return Nothing
interpret st cmd | otherwise = do
  putStrLn $ "Error: illegal command `" ++ cmd ++ "'"
  return $ Just st

printStack st = putStrLn $ show st

validCmd cmd = cmd `elem` ["add", "substract", "pop", "+", "-", "*", "dup", "multiply" ] || isInt cmd
