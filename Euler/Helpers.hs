module Euler.Helpers (
 toDigits
 ,primes
 ,isPrime
 ,fromDigits
) where

toDigits 0 = []
toDigits n = (num n):(toDigits (n `div` 10))
    where
        num n = n - (n `div` 10)*10
fromDigits xs = foldr1 d (reverse xs)
    where d a b = a + b*10
primes = 2:[x|x<-[3,5..], isPrime x]
isPrime n = fst (head (divisors n)) == 1
sqr n = floor (sqrt (fromInteger n))

divisors n = [(x, x `div` n) | x<-[sqr n, sqr n - 1..1], n `mod` x == 0]
