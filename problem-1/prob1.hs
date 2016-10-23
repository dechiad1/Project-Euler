{-|
   project euhler problem 1
   find the sum of all multiples of 3 and 5 beneath 1000
-}
main = print(multiplesOf3And5Sums 1000)

filterList :: Int -> [Int]
filterList n = [x | x <- [1..(n-1)], mod x 3 == 0 || mod x 5 == 0]

sumList :: [Int] -> Int
sumList n = sum n

multiplesOf3And5Sums :: Int -> Int
multiplesOf3And5Sums n = sumList (filterList n)

