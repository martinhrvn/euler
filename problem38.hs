import Euler.Helpers
import qualified Data.List as L
isPandigital n = length n == 9 && all (`elem` n) [1..9]

multiples n = [x*n | x<-[1..]]

takeNumbers n = takeNumber (toDigits n) n 2
    where
        takeNumber xs n i
            | 0 `elem` xs = []
            | xs /= L.nub xs = []
            | length xs >= 9 = xs
            | otherwise = takeNumber (xs ++ toDigits (n*i)) n (i+1)

getNumbers n = take 9 (concatMap toDigits (multiples n))
problem38 = maximum [fromDigits $ takeNumbers x | x <- [1..329218107], isPandigital $ takeNumbers x]

main = print problem38
