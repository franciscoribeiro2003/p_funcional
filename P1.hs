module Aula1 where

 incr, triplo :: Integer -> Integer
 incr x = x+1
 triplo x = 3*x
 boasVindas :: String -> String
 boasVindas nome = "Olá, " ++ nome ++ "!"

 {-
 1a)
 incr (triplo 3)
 incr (3*3)
 incr 9
 9 + 1
 10

 1c) boasVindas "Linguagem" ++ " Haskell"
 ("Olá, " ++ "Linguagem" ++ "!") ++ " Haskell"
 ("Olá Linguagem" ++ "!") ++ " Haskell"
 ("Olá, Linguagem!") ++ "Haskell"
 "Olá, Linguagem! Haskell"
 -}

 --1.2)

 testaTriangulo :: Float -> Float -> Float -> Bool
 testaTriangulo a b c = (( a < b+c)&&(b < c+a)&&(c < a+b))

 testaTriangulo' :: Float -> Float -> Float -> Bool
 testaTriangulo' a b c
  | a >= b + c = False
  | b >= a + c = False
  | c >= a + b = False
  | otherwise = True


 --1.3)

 areaTriangulo :: Float -> Float -> Float -> Float
 areaTriangulo a b c = sqrt (s*(s-a)*(s-b)*(s-c))
  where
   s = (a + b + c)/2
 areaTriangulo' :: Float -> Float -> Float -> Float
 areaTriangulo' a b c =
  let s= (a + b + c)/2 in
   sqrt (s*(s-a)*(s-b)*(s-c))

 --1.4)
 metades :: [a] -> ([a],[a])
 metades l  = (left, right)
  where
   left = take n l
   right = drop n l
   n = div(length l) 2

 --1.5)
 last' :: [a] -> a
 last' l = head (reverse l)

 last'' :: [a] -> a
 last'' l = head (drop (length l - 1) l)

 init' :: [a] -> [a]
 init' l = reverse (tail (reverse l))
 
 init'' :: [a] -> [a]
 init'' l = take (length l-1) l

 --1.6)a)
 binom :: Integer -> Integer -> Integer
 binom n k = div num den
  where
   num = product [1..n]
   den = product [1..k] * product [1..(n-k)]
