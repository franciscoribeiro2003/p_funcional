elefantes :: Int -> IO ()
elefantes n = auxiliar 2 n

auxiliar :: Int -> Int -> IO ()
auxiliar k n |(k==n) = return ()
             | otherwise = do putStrLn ("Se " ++ show k ++ " elefantes incomodam muita gente,")
                              putStrLn ( show (k+1) ++ " elefantes incomodam muito mais!")
                              auxiliar (k+1) n
