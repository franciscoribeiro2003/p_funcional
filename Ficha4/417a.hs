merges :: Ord a => [a] -> [a] -> [a]
merges [] [] = []
merges [xs] [] = [xs]
merges [ys] [] = [ys]
merges (x:xs) (y:ys) | x<=y = [x] ++ merges xs (y:ys)
                     | x>y = [y] ++ merges (x:xs) ys

mssort :: Ord a => [a] -> [a]
mssort [] = []
mssort [x] =[x]
mssort xs = merges (mssort(parte xs)) (mssort (parte2 xs))
 where
tamanho = length xs `div` 2
parte   = take tamanho xs
parte2  = drop tamanho xs

metades :: [a] -> ([a],[a])
metades lista = (take x lista, drop x lista)
        where x = div (length lista) 2