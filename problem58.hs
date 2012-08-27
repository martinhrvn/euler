import Euler.Helpers
import qualified Data.List as L
takeWhileArr f xs = takeWhileF f [] xs
    where
        takeWhileF f rs [] = reverse rs
        takeWhileF f rs (x:xs)
            | f (x:rs) = takeWhileF f (x:rs) xs
            | otherwise = reverse rs

over10P = takeWhileArr o10pp (map isPrime diagonals)
    where 
        o10pp xs 
            | null xs = False
            | (length xs - 1) `mod` 4 == 0 = fromIntegral (length . head $ grps xs) / fromIntegral (length xs) < 0.1       
            | otherwise = True
        grps = L.group . L.sort

main = print ( length ( head ( L.group (L.sort (take 100 (map isPrime diagonals))))))
