insert :: Ord a => a -> [a] -> [a] 
insert a [] = [a]
insert a (x:xs) | a<=x = a:x:xs
                | otherwise = x:(insert a xs)
               

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)