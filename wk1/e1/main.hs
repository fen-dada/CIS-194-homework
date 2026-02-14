toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : (y : res)) = [x] ++ [2 * y] ++ doubleEveryOther res

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : res)
  | n < 10 = n + sumDigits res
  | otherwise = mod n 10 + div n 10 + sumDigits res

validate :: Integer -> Bool
validate n = mod (sumDigits (doubleEveryOther (toDigitsRev n))) 10 == 0
