triangle n = n * (n -1) `div` 2
penta n = n * (3*n - 1) `div` 2
hexa n = n * (2*n - 1)

hexas = map hexa [1..]
pentas = map penta [1..]
trias = map triangle [1..]

isPentag n = n `elem` takeWhile (<= n) (map penta [1..])
isTriangular n = n `elem` takeWhile (<= n) (map triangle [1..])

problem45 = head [x | x<-hexas, x>40755, isPentag x, isTriangular x]

main = print problem45
