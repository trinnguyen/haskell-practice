-- example: toDigitsRev 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
 | n < 0 = []
 | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- double every other number from right, second-to-list, fourth-to-last --
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x: y * 2 : doubleEveryOther(xs)

-- calculate of all digits --
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs)
 | x < 10 = x + sumDigits(xs)
 | otherwise = sumDigitsSingle(x) + sumDigits(xs)

-- Util sum all digits of a single number --
sumDigitsSingle :: Integer -> Integer
sumDigitsSingle n
 | n < 10 = n
 | otherwise = (n `mod` 10) + sumDigitsSingle(n `div` 10)

-- Validate the number
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigitsRev n) `mod` 10 == 0
