fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs) = x * 2 ^ (length xs) foldl + fromBits xs
