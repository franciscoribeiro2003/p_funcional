palavras :: String -> [String]
palavras " " = []
palavras (x:xs) | x == ' ' = []
                | otherwise = primeira : palavras resto
             where primeira = dropWhile()
                   resto = 
