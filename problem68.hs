import Data.List (permutations)

nums = filter equalSums (permutations [1..6])
   where 
    equalSums xs = (sm xs 0 1 5 == sm xs 1 2 3) && (sm xs 0 1 5 == sm xs 2 0 4)
    sm xs a b c = xs!!a + xs!!b + xs!!c

stings = [ar nums 3 2 1, ar nums 4 0 2, ar nums 5 1 0]
    ar xs a b c = [xs!!a, xs!!b, xs!!c]

