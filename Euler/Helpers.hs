module Euler.Helpers (
 toDigits
 ,primes
 ,isPrime
 ,fromDigits
 ,divisors
 ,isPalindrome
) where

toDigits 0 = []
toDigits n = toDigits (n `div` 10)++[num n]
    where
        num n = n - (n `div` 10)*10
fromDigits xs = foldr1 d (reverse xs)
    where d a b = a + b*10
primes = 2:[x|x<-[3,5..], isPrime x]
isPrime n = fst (head (divisors n)) == 1
sqr n = floor (sqrt (fromInteger n))
isPalindrome n = n == reverse n

divisors n = [(x, n `div` x) | x<-[sqr n, sqr n - 1..1], n `mod` x == 0]
