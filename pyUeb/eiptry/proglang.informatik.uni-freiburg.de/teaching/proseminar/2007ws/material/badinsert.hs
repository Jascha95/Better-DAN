sort :: Ord a => [a] -> [a]
sort (x:xs) = insert x (sort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) = if x <= y
			then x: ys
			else y: insert x (y:ys)

main = putStrLn (sort "program")