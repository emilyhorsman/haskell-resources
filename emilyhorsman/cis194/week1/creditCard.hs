toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

alternateMapRight :: (a -> a) -> [a] -> [a]
alternateMapRight f [] = []
alternateMapRight f [x] = [x]
alternateMapRight f (x:y:zs)
    -- [1,2] with [3,4] remaining -> first number should be mapped
    -- [1,2] with [3,4,5] remaining -> second number should be mapped
    | isEven (length zs) = f x : y : alternateMapRight f zs
    | otherwise          = x : f y : alternateMapRight f zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = alternateMapRight (* 2) xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:zs) = sumDigits (toDigits x) + sumDigits zs

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
