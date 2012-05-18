import qualified Data.List as L
import Euler.Helpers
shifts [] = []
shifts xs = L.nub (zipWith concat (L.tails xs) (L.inits xs))
    where 
        concat a b = a ++ b
problem35 = 2:3:5:[x | x<-(takeWhile (<1000000) primes), (sum (toDigits x)) `mod` 3 /= 0, notEq [2,4,5,6,8,0] (toDigits x), all (isPrime) (map fromDigits (shifts (toDigits x)))]
    where 
       notEq [] ys = True 
       notEq (c:cs) ys = all (/=c) ys && notEq cs ys
main = print (length problem35)
