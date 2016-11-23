import Data.List (length)

myLength (h:tl) = 1 + myLength tl
myLength [] = 0

main = print $ myLength [1,32,3, 2]
