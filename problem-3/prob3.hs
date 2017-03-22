{-|
 Project Euler problem 3
 find the largest prime factor
-}

greatestPrime :: (Integral a) => a -> a
greatestPrime n = head [x | x <- reverse [1..(floor $ (/) (fromIntegral n) 2)], isFactor n x && isPrime x]

isFactor :: (Integral a) => a -> a -> Bool
isFactor a b
  | mod a b == 0 = True
  | otherwise    = False

--use fromIntegral; as their are no default number types that satisfy all constraints
isPrime :: (Integral a) => a -> Bool
isPrime a 
  | mod a 2 == 0                                = False
  | mod (floor $ (/) (fromIntegral a) 2) 2 == 0 = isPrime' a ((floor $ (/) (fromIntegral a) 2) - 1)
  | otherwise                                   = isPrime' a (floor $  (/) (fromIntegral a) 2)  

isPrime' :: (Integral a) => a -> a -> Bool
isPrime' x y
  | x       == 1 = True
  | y       == 1 = True
  | mod x y == 0 = False
  | mod x y /= 0 = isPrime' x (y-2) 
  | otherwise    = False

