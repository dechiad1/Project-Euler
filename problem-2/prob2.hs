{-!
 Project Euler problem 2. Add all even fibonacci numbers less than 4000000
-}

main = print(sumEvenFibsLessThan 4000000)

sumEvenFibsLessThan :: Integer -> Integer
sumEvenFibsLessThan n = sum (filter even (takeWhile (<n) fibs))

fibs = 1:1:zipWith (+) fibs (tail fibs)

