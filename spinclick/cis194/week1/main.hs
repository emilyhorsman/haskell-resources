import Data.Char (digitToInt)

number = map digitToInt "4012888888881882"
--number = map digitToInt "1386"

pmap :: [a] -> [(a,a)]
pmap (f:s:rest) = (f,s):(pmap rest)
pmap [] = []

splitDigit :: Int -> [Int]
splitDigit i 
  | i == 0 = []
  | otherwise = (i `mod` 10) : (splitDigit $ i `div` 10)

doubleSnd :: [(Int,Int)] -> [Int]
doubleSnd ((f,s):rest) = f:s:(doubleSnd rest)
doubleSnd [] = []

answer = ((sum $ doubleSnd $ pmap $ reverse $ number) `mod` 10) == 0

main = print answer
