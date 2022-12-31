minimu :: Ord a => [a] -> a
minimu [x] = x
minimum (x:xs) | (x<minimu xs) =x
               | otherwise = minimu xs