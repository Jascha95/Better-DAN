import Test.QuickCheck

power x n | n == 0 = 1
power x n | n > 0 = x * power x (n - 1)

                    
prop_power (x, n1, n2, n3)  =
  (n1 >= 0 && n2 >= 0 && n3 >= 0) ==>
  power x (n1 * n2 * n3) == power (power (power x n1) n2) n3
