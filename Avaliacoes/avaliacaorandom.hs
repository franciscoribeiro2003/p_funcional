decompor::Integer->[Integer]
decompor 0=[]
decompor x = decompor (x `div` 10) ++ [x `mod` 10]

duplicar::[Integer]->[Integer]
duplicar []=[]
duplicar (x:y:xs)|length (x:y:xs) `mod` 2==0 = (x*2: y: duplicar xs)
duplicar (x:y:z:xs)= (x:y*2:z:duplicar xs)

soma::[Integer]->Integer
soma[]=0
soma (x:xs) |(x>9)= ((x`div`10)+(x`mod`10) + soma xs)
            | otherwise = x + soma xs


validar :: Integer-> Bool
validar x |((soma (duplicar (decompor x))) `mod` 10 ==0) = True
          |otherwise = False
