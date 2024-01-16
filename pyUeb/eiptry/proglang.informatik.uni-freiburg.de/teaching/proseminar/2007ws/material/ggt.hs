ggT :: Int -> Int -> Int
ggT a b = if (a < b)	then ggT b a
			else let x = a `mod` b in
			if (x == 0) then b
			else ggT b x