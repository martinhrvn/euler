import qualified Data.List as L
import Euler.Helpers
shifts [] = []
shifts xs = L.nub (zipWith concat (L.tails xs) (L.inits xs))
    where 
        concat a b = a ++ b
problem35 = [x | x<-primes, x<1000000, all (isPrime) (map fromDigits (shifts (toDigits x)))]
main = print (problem35)
