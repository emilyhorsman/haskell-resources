totient :: Int -> Int
totient m = length $ filter (\i -> (gcd m i) == 1) [1..(m-1)]

main = print $ totient 10
