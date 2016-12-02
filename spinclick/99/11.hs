data RLEData a = 
    Single a 
  | Multiple Int a 
    deriving Show

makeRLE :: Int -> a -> RLEData a
makeRLE len itm
  | len > 1   = Multiple len itm
  | otherwise = Single itm

encodeModified :: (Eq a) => [a] -> [RLEData a]
encodeModified lst
  | length lst <= 0 = []
  | otherwise = 
    let itm = (head lst)
        len = length $ takeWhile (== itm) lst 
    in
    makeRLE len itm : (encodeModified (drop len lst))

main = print $ encodeModified "aaaabccaadeeee"
