import Euler.Helpers
import qualified Data.List as L

nums n = map (*n) [1..6]
sameN xs = all (== head (digits xs)) (digits xs)
    where digits xs = map (L.sort . toDigits) xs

main = print . head .filter (sameN) $ map (nums) [1..] 
