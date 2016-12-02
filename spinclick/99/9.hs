inLastPack :: (Eq a) => a -> [[a]] -> Bool
inLastPack itm packs 
  | length packs > 0 = itm `elem` (head packs)
  | otherwise = False

packIfSame :: (Eq a) => a -> [[a]] -> [[a]]
packIfSame itm packs
  | inLastPack itm packs = (itm : (head packs)) : (tail packs)
  | otherwise = [itm] : packs

pack :: (Eq a) => [a] -> [[a]]
pack lst =
  foldr packIfSame [] lst

main = print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] 
