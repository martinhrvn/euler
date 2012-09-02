import Euler.Helpers
import qualified Data.List as L

prime n = takeWhileSum n primes
takeWhileSum n = takeWhileArr (\x -> sum x <= n)
takeWhileArr f xs = takeWhileF f [] xs
    where
        takeWhileF f rs [] = reverse rs
        takeWhileF f rs (x:xs)
            | f (x:rs) = takeWhileF f (x:rs) xs
            | otherwise = reverse rs

primeSums n = map (map (\x -> (isPrime x,x) ) . takeWhile (<n) . scanl1 (+)) (L.tails (prime n))
main = print . maximum $ map index (primeSums 100000)
    where index x = if null $ ind x 
                    then (0,0) 
                    else (last $ ind x, snd (x !! last (ind x)))
          ind = L.findIndices fst
