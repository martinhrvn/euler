import Euler.Helpers
import qualified Data.List as L
import Control.Applicative

diagonals = scanl1 (+) (1 : liftA2 (*) (map (*2) [1..]) [1,1,1,1])
takeWhileArr f xs = takeWhileF f [] xs
    where
        takeWhileF f rs [] = reverse rs
        takeWhileF f rs (x:xs)
            | f (x:rs) = takeWhileF f (x:rs) xs
            | otherwise = reverse rs

over10P = takeWhileArr o10pp (map isPrime diagonals)
    where 
        o10pp rs 
            | null rs = False
            | length rs > 1 && (length rs - 1) `mod` 4 == 0 = fromIntegral (length . head $ grps rs) / fromIntegral (length rs) < 0.9       
            | otherwise = True
        grps = L.group . L.sort

main = print . length $ over10P
