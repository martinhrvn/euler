import Euler.Helpers
import qualified Data.List as L

sums xs = L.scanl1 (+) xs
isSum n = len (filter (elem n) (map (\x -> takeWhile (<=n) (sums x)) (L.tails (takeWhile (<n) primes))))
    where 
        len [] = Just 0
        len xs = L.elemIndex n (head xs)
takeWhileArr f xs = takeWhileF f [] xs
    where
        takeWhileF f rs [] = rs
        takeWhileF f rs (x:xs)
            | f (x:rs) = takeWhileF f (x:rs) xs
            | otherwise = rs
--main = print (maximum (map (\x -> (isSum x, x)) (takeWhile (<1000000) primes)))
--main = print (maxLength (reverse (takeWhile (<1000000) primes) ) (0,0) )
allSums n = filter (\x -> length x > 6 && sum x < n && not (null x) && isPrime (sum x)) (L.nub . L.concat $ map L.tails (L.inits $ takeWhileArr (\x -> (sum x) < n) primes))
main = print (head (L.sortBy (\x y -> compare (fst y) (fst x)) (map (\x -> (length x, x) ) (allSums 1000000))))
