fac n = product [1..n]

numbers 0 = []
numbers n = (num n):(numbers (n `div` 10))
    where
        num n = n - (n `div` 10)*10

sumFact n = sum (map fac (numbers n) )

problem34 = sum [(sumFact n, n) | n <- [3..10000000], sumFact n == n]
main = print (problem34)
