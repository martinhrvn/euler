import Euler.Helpers
import qualified Data.List as L

pandigital = filter (\a -> head a /= 0) (L.permutations [0..9])

divis n a d = (fromDigits ((n !! (a-1)) : (n !! a) : [(n !! (a+1))])) `mod` d == 0

problem43 = sum [fromDigits x | x<- pandigital, and $ zipWith (divis x) [2..8] [2,3,5,7,11,13,17]]  

main = print problem43
