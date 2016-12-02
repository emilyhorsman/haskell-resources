import Data.List (head)

growIfNotFound :: Char -> [Char] -> [Char]
growIfNotFound ch lst
  | ch `elem` lst && (head lst) == ch = lst
  | otherwise = ch:lst

compress :: [Char] -> [Char]
compress str = foldr (growIfNotFound) "" str

main = print $ compress "aaaabccaadeeee"
