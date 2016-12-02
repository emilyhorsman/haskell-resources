encode :: (Eq a) => [a] -> [(Int,a)]
encode lst
  | length lst == 0 = []
  | otherwise = 
    let itm = (head lst)
        group = takeWhile (== itm) lst
        groupLen = length group
    in
    (groupLen,itm) : (encode (drop groupLen lst))

main = print $ encode "aaaabccaadeeee"
