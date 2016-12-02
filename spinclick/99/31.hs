dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly p n = p `mod` n == 0

isPrime :: Integer -> Bool 
isPrime p = not $ any (dividesEvenly p) [2..(p-1)]

main = print $ isPrime 7
