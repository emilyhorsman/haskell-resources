data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List inner) = concat $ map (\e -> flatten e) inner
flatten (Elem i) = [i]

main = print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
