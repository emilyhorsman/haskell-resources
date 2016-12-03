dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly p n = p `mod` n == 0

isPrime :: Integer -> Bool 
isPrime p = not $ any (dividesEvenly p) [2..(p-1)]

printIfPrime :: Integer -> IO ()
printIfPrime p 
  | isPrime p = print p
  | otherwise = return ()

findPrimes :: Integer -> IO ()
findPrimes i = printIfPrime i >> findPrimes (succ i)

main = findPrimes 2
