import Prelude hiding (zipWith)
-- esconder a definição do prelúdio

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys)= f x y : zipWith f xs ys
zipWith _ _  _ = []
