import Data.Maybe (maybe, catMaybes)
import Data.List (find)
import Data.String (unlines)

occurances needles haystack =
  let findFn word = find (\w -> w == word) haystack 
  in
  print ("words from needles in haystack: " ++ (unwords $ catMaybes $ map findFn needles))

main = 
  readFile "needles.txt" >>= \needles ->
  readFile "haystack.txt" >>= \haystack ->
  occurances (words haystack) (words needles)
