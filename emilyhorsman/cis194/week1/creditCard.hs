{- |
Description : CIS 194 Homework 1
-}


-- | Take a positive number and return a list of its digits in reverse order.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)


-- | Take a positive number and return a list of its digits.
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


-- | Determine if a number is even.
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0


-- | Apply a function to every other item a list, starting from second last
--   item in the list. This means in a list of five items, the function will be
--   applied to the 1st and 3rd item.
--
--   [1, 2, 3, 4, 5] => [1, f 2, 3, f 4, 5]
alternateMapRight :: (a -> a) -> [a] -> [a]
alternateMapRight _ [] = []
alternateMapRight _ [x] = [x]
alternateMapRight f (x:y:zs)
    -- [1,2] with [3,4] remaining -> first number should be mapped
    -- [1,2] with [3,4,5] remaining -> second number should be mapped
    | isEven (length zs) = f x : y : alternateMapRight f zs
    | otherwise          = x : f y : alternateMapRight f zs


-- | Double every other number in a list.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = alternateMapRight (* 2)


-- | Given a list of numbers, sum all the digits of all the numbers.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:zs) = sumDigits (toDigits x) + sumDigits zs


-- | Determine if a number is a valid credit card number.
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
