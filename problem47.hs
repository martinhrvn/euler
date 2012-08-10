import Euler.Helpers
import qualified Data.List as L
first f xs = head [x | x<-xs, f x]
primeFactors 1 = []
primeFactors n = firstPrime:primeFactors (n `div` firstPrime)
    where
        firstPrime = first (\x -> n `mod` x == 0) primes

distinctFactors n = L.nub (primeFactors n)

problem47 = head [x | x <- [644..], cond x, cond (x+1), cond (x+2), cond(x+3)]
    where cond x = length (distinctFactors x) == 4

main = print problem47
