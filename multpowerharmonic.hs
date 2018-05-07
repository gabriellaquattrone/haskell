-- Author: Gabriella Quattrone
-- PROBLEM 1
-- Multiplies using repeated addition
-- ONLY USE NON-NEGATIVE INTEGERS
multiply :: Int -> Int -> Int 
multiply x y
   | y == 1            = x
   | y == 0            = 0
   | otherwise         = x + multiply x (y-1)

-- PROBLEM 2
-- Multiplies by using the second input as a counter as to how many times the first
-- input should be added, then stores the result in the variable called "adder"
-- ONLY USE NON-NEGATIVE INTEGERS 
multiply_tr :: Int -> Int -> Int 
multiply_tr x y = multiply_tr_helper x y 0

multiply_tr_helper :: Int -> Int -> Int -> Int
multiply_tr_helper x y adder
    | y == 0             = adder
    | otherwise          = multiply_tr_helper x (y - 1) (adder + x)

-- PROBLEM 3 
-- Non Tail Recursive Power Function
-- The reason why this solution is not tail recursive is because its outermost function is not itself (power)
-- ONLY USE NON-NEGATIVE INTEGERS
power :: Int -> Int -> Int
power first second
     | second == 0                  = 1
     | second == 1                  = first
     | otherwise                    = multiply first (power first (second - 1))

-- PROBLEM 4
-- Tail Recursive Power Function
-- Use second to act as a counter as to how many times first should be multiplied
-- For example, if second = 3 and first = 5, first should be multiplied upon itself 3 times as in 5*5*5
-- To do this, we decrement second by 1 every time we pass it into the power function.
-- Then we save the result of power into the second argument of the tail-recursive version of multiply.
-- ONLY USE NON-NEGATIVE INTEGERS
power_tr :: Int -> Int -> Int
power_tr first second = power_tr_helper first second 1

power_tr_helper :: Int -> Int -> Int -> Int
power_tr_helper first second product
     | second == 0                  = product
     | otherwise                    = power_tr_helper first (second - 1) (multiply_tr first product)

-- PROBLEM 5
-- Non Tail Recursive Version of Harmonic Series Calculator
-- Use Double to accept Integers that later turn into Floats/Doubles (Doubles are more precise.)
-- ONLY USE AN INTEGER GREATER THAN 0
harmonic :: Double -> Double
harmonic n
     | n == 1      = 1
     | n > 1       = (1/n) + harmonic (n-1)

-- PROBLEM 6
-- Tail Recursive Version of Harmonic Series Calculator
-- Solution: Store the output in the variable result and pass it through each function call as it is added upon
-- ONLY USE AN INTEGER GREATER THAN 0
harmonic_tr :: Double -> Double
harmonic_tr n = harmonic_tr_helper n 1

harmonic_tr_helper :: Double -> Double -> Double
harmonic_tr_helper n result
    | n == 1       = result
    | n > 1        = harmonic_tr_helper (n-1) (result + 1/n)
