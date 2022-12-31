--2.1)
classifica :: Int-> String
classifica a | a>=0 && a<=9   = ("reprovado")
             | a<=12          = ("suficiente")
             | a<=15          = ("bom")
             | a<=18          = ("muito bom")
             | a<=20          = ("muito bom com distinção")

--2-2)
classifica_imc :: Float -> Float -> String
classifica_imc kg metros | imc<18.5 = ("baixo peso")
                         | imc<25   = ("peso normal")
                         | imc<30   = ("excesso de peso")
                         | imc>=30  = ("obesidade")
                    where
                     imc=kg/(metros*metros)
--2.3)
---a)
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z | x>=y && x>=z = (x)
           | y>=x && y>=z = (y)
           | z>=x && z>=y = (z)

min3 x y z | x<=y && x<=z = (x)
           | y<=x && y<=z = (y)
           | z<=x && z<=y = (z)

---b)
max31, min31 :: Ord a => a -> a -> a -> a
max31 x y z = max (max x y) z
min31 x y z = min (min x z) z

--2.4)
xor :: Bool -> Bool -> Bool
xor x y | x==False && y ==True  = (True)
        | x==False && y ==False = (False)
        | x==True  && y ==True  = (False)
        | x==True  && y ==False = (True)

--2.5)
safetail :: Eq a => [a] -> [a]
safetail a | a== [] = []
           |otherwise = tail a

--2.6)
curta :: [a] -> Bool
curta [a]|length[a]==0  = (False)
         |length [a]>0  = (True)

--2.7)
mediana :: Ord b => b -> b -> b -> b
mediana x y z | (x>=y && x<=z) || (x<=y && x>=z) = (x)
              | (y>=x && y<=z) || (y<=x && y>=z) = (y)
              | (z>=x && z<=y) || (z<=x && z>=y) = (z)
