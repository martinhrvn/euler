primes = 2:[x | x<-[3,5..], isPrime x]

isPrime n = fst (head (divisors n)) == 1

sqr :: Integer -> Integer
sqr n = floor (sqrt (fromInteger n))

divisors n = [(x,n `div` x) | x<-[sqr n, sqr n - 1..1], n `mod` x == 0] 

main = print (maximum (primeFactors 600851475143))

first f xs = head [x | x<-xs, f x]
primeFactors 1 = []
primeFactors n = firstPrime:primeFactors (n `div` firstPrime)
    where
        firstPrime = first (\x -> n `mod` x == 0) primes

problem03 n = max [x | x<-primes, n `mod` x == 0 ]
