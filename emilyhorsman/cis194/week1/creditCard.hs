toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs)
    -- [1,2] with [3,4] remaining -> first number should be doubled
    | length zs `mod` 2 == 0 = (x * 2) : y : doubleEveryOther zs
    -- [1,2] with [3] remaining -> second number should be doubled
    | otherwise = x : (y * 2) : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:zs) = sumDigits (toDigits x) + sumDigits zs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
