{-decompor::Int->[Int]
moedas = [200, 100, 50, 20, 10, 5]
decompor x =decompor1 x moedas

decompor1::Int->[Int]->[Int] 
decompor1 0 x = []
decompor1 x (x:xs) | n>x || n==[x] = (c `div` x)*n ++ decompor1 (n-x) 
                   | otherwise= decompor1 n xs 
-}

--coinChange c ds = reverse (coinChange' c (reverse ds))


decompor::Int->[Int]
decompor x = troco x [200, 100, 50, 20, 10, 5]

troco:: Int->[Int]->[Int]
troco 0 x = []
troco 1 x = []
troco 2 x = []
troco 3 x = []
troco 4 x = []
troco c (x:xs) | (c `div` x)>1 = x : troco ((c `div` x)-1) xs
               | (c `div` x) == 1 = x: troco (c `mod` x) xs
               | (c `div` x)==0 = []: troco (c `mod` x)


