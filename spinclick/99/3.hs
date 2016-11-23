import Data.List

elementAt :: [a] -> Int -> a
elementAt lst at = 
   head $ drop (at - 1) lst

main = print $ elementAt "hello world!" 2
