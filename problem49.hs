import Euler.Helpers
import qualified Data.List as L
fourDigitPrimes = filter (>1000) (takeWhile (<9999) primes)

problem49 = take 2 [(x,y) | x <- fourDigitPrimes, y <- [2,4..9999], isPrime (x+y), isPrime (x+2*y), L.sort (toDigits x) == L.sort (toDigits (x+y)), L.sort (toDigits x) == L.sort (toDigits (x+2*y))]

main = print problem49
