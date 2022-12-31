insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a]
insert a (x:xs) | a<=x = a:x:xs
                | otherwise = x:(insert a xs)
               