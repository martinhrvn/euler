pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal n = n `elem` takeWhile (<=n) pentagonals
pentagonals = map pentagonal [1..]

problem44 = head [-(y - x) | x<- pentagonals, y<- reverse ( takeWhile (<x) pentagonals ), isPentagonal(x-y), isPentagonal(x+y) ]

main = print problem44
