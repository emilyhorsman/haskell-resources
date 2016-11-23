butLast :: [a] -> a
butLast lst =
   case lst of
      (h:l:[]) -> h
      (h:r) -> butLast r

main = print $ butLast [1,3,4,5]
