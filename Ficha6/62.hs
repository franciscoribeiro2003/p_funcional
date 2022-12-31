module Main where
main :: IO ()
main = do
  ficheiro <- getContents
  putStr(show(length(lines ficheiro)) ++ "\n" ++ show(length(words ficheiro)) ++ "\n" ++ show(length ficheiro))
