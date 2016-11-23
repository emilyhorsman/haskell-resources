myReverse (hd:tl) = (myReverse tl) ++ [hd, hd] 
myReverse [] = []

main = print $ myReverse [1, 2, 3]
