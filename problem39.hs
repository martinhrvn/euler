problem39 = maximum [(length (solutions x), x) | x<-[1000,999..1]]
    where
        solutions n = [(x,y,n-x-y) | x<-[4..n-2], y<-[1..(n-x-1)], x*x == y*y + (n-x-y)*(n-x-y)]
        sqr n = floor (sqrt (fromIntegral n))

main = print problem39
