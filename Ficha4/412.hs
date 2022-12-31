

toBits :: Int -> [Int]
toBits n = reverse(toBitsRev n)

toBitsRev :: Int -> [Int]
toBitsRev 0 = []
toBitsRev n = n`mod`2 : toBitsRev (n`div`2)