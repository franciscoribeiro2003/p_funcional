import Data.Char
-- 3.1 ao 3.5, 3.8, 3.9

--3.1)
divprop :: Integer -> [Integer]
divprop n = [x | x<-[1..(n-1)], n`mod`x==0]

--3.2)
perfeitos :: Integer -> [Integer]
perfeitos n = [x|x<-[1..(n-1)], (sum (divprop x))==x]

--3.3)
pitagoricos :: Int->[(Int,Int,Int)]
pitagoricos n =[(x,y,z)|x<-[1..n], y<-[1..n],z<-[1..n],(x*x+y*y)==(z*z)]

--3.4)
divpropx :: Integer -> [Integer]
divpropx n = [x | x<-[1..(n)], n`mod`x==0]

primo :: Integer->Bool
primo n = divpropx n == [1,n]

--3.5)
binom :: Integer -> Integer -> Integer
binom n k = div num den
 where
  num = product [1..n]
  den = product [1..k] * product [1..(n-k)]

linhasoma::Integer->[Integer]
linhasoma l=[binom l k|k<-[0..l]]
--expor à esquerda os valores que queremos colecionar
--se fosse à direita, era uma condição (ex:linha n = [k <- [0..n] | binom n k])

pascal :: Integer->[[Integer]]
pascal n=[linhasoma k|k <- [0..(n)]]

--3.6
--ord= codigo unicode de um caracter
--Chr= caracter de um unicode

--convert letra to unicode
letraint:: Char -> Int
letraint x =ord x - ord 'A'
--convert unicode to letra
intletra:: Int -> Char
intletra n=chr (ord 'A' + n)

--deslocar letras
shift:: Int->Char->Char
shift k x |maiscula x = intletra((mod) (letraint x + k) 26)
          |otherwise = x

maiscula:: Char->Bool
maiscula x=x>='A' && x<='Z'

cifrar :: Int -> String -> String
cifrar k xs = [shift k x |x <- xs]


--3.7)
myand:: [Bool]->Bool
myand [] = True
myand (False:xs) = False
myand (True:xs)  = myand xs

myor:: [Bool]->Bool
myor [] = False
myor (True:xs) = True
myor (False:xs) = myor xs
