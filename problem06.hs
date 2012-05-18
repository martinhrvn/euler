nums = [1..100]
problem06 = sq (sum nums) - sum (map sq nums)
    where
        sq n = n*n
main = print problem06
