{- |
Description: CIS 194 Homework 1
-}


type Peg = String


type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 2 from to buffer =
    [ (from, buffer)
    , (from, to)
    , (buffer, to)
    ]
hanoi height from to buffer = concat
    [ hanoi (height - 1) from buffer to
    , [(from, to)]
    , hanoi (height - 1) buffer to from
    ]
