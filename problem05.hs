lcm' n m = (n * m) `div` gcd' n m
gcd' n m
    | n == m = n
    | n >  m = gcd' (n-m) m
    | n <  m = gcd' n (m-n)

problem05 = foldl (lcm') 1 [1..20]
main = print( problem05 )
