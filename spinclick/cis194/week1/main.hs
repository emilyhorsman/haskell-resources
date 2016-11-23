import Data.Char

number = map digitToInt "4012888888881881"

pzip :: [a] -> [(a,a)]
pzip (a:b:rest) = (a,b) : (pzip rest)
pzip [] = []

doubleEveryFromRight :: [Int] -> [Int]
doubleEveryFromRight card = 
  unzip $
    map 
      (\p -> ((snd p)*2, (fst p))) 
      (pzip (reverse card))

main = print $ (doubleEveryFromRight [1,3,8,6])

