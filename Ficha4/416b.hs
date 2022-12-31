dele :: Eq a => a -> [a] -> [a]
dele _ [] = []
dele a (y:ys) | a==y = ys
              | otherwise = y:( dele a ys)


ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort a = x: ssort(dele a x)
          where x = minimu a