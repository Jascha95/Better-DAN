import Test.QuickCheck

dollarRate :: Double
dollarRate = 1.3673

price :: Double
price = 79

-- convert euros to USD
usd :: Double -> Double
usd x = x * dollarRate

-- convert USD to EUR
euro :: Double -> Double
euro x = x / dollarRate

-- testing euro and usd for inverse
prop_EuroUSD :: Double -> Bool
prop_EuroUSD x = x == 0 || euro (usd x) ~== x

-- almost equal for floating point
(~==) :: Double -> Double -> Bool 
x ~== y = abs (x - y) < 10e-15 * abs x

-- returns the absolute value of x
absolute :: Integer -> Integer
absolute x = if x >= 0 then x else - x

-- compute x to n-th power
power x 0         = 1
power x n | n > 0 = x * power x (n - 1)

power' x n = if n==0 then 1 else x * power x (n - 1)

prop_power_is_power' x n = n < 0 || power x n == power' x n

n_intersect :: Integer -> Integer
n_intersect 0 = 0
n_intersect n | n > 0 = n-1 + n_intersect (n-1)

