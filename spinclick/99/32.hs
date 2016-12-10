myGCD a b 
  | b == 0 = a
  | otherwise = myGCD b (a `mod` b)

main = print $ myGCD 36 63
