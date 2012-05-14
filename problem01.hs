main = print (problem01)

problem01 = sum [n | n <- [2..1000-1], n `mod` 5 == 0 || n `mod` 3 == 0]
