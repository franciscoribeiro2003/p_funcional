
algarismos :: Int -> [Int]
algarismos n = reverse(algarismoRev n)


algarismoRev :: Int -> [Int]
algarismoRev 0 = []
algarismoRev n = n`mod`10 : algarismoRev (n`div`10)

