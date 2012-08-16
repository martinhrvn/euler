import Euler.Helpers
import qualified Data.List as L

maxPossibleSum n = maxSum 0 (takeWhile (<n) primes) n
    where
        maxSum _ [] _ = 0
        maxSum acc (x:xs) n
            | acc+x > n = 0
            | otherwise = 1 + maxSum (acc+x) xs n
possibleSums n = L.tails (takeWhile (<n) primes) 
maxLength [] ml = ml
maxLength (x:xs) ml
    | maxPossibleSum x <= fst ml = ml
    | countLength x > ml = maxLength xs (countLength x)
    | otherwise =  maxLength xs ml
    
countLength n = sumsArray n (head (possibleSums n))
    where 
        sumsArray n x = (maxL 0 n x, n)
        maxL acc n [] = 0
        maxL acc n (x:xs)
            | acc+x == n = 1
            | acc+x > n = 0
            | otherwise = if maxL (acc+x) n xs == 0 then 0 else 1+(maxL (acc+x) n xs)
main = print (countLength 997651)
--main = print (maxLength (reverse (takeWhile (<1000000) primes) ) (0,0) )
