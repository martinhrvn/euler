import System.Random
primes = 2:[x|x<-[3,5..], isPrime x]
primes' = 2:[x|x<-[3,5..], rabinmiller x]

isPrime n = fst (head (divisors n)) == 1

sqr n = floor (sqrt (fromInteger n))

divisors n = [(x, x `div` n) | x<-[sqr n, sqr n - 1..1], n `mod` x == 0]

main = print ( primes' !! 100000)

fun2mk :: Integer->(Integer,Integer)
fun2mk n = let 
        (s,d)= f 0 (n-1) where 
              f s d | mod d 2 ==0 = f (s+1) (div d 2)
                | otherwise = (s,d)    
       in (s,d)
rabinmiller :: Integer->Bool
rabinmiller n   | n<2 = False
        | n==2 = True
        | even n = False
        | b0==1 || b0== n' = True
        | otherwise = iter (tail b)
        where 
            n'= n-1
            (k,m)=fun2mk n
            a= head ( take 1 $ randomRs (2,n-2) (mkStdGen 3) :: [Integer])
            b0 = mod (a^m) n
            b= take (fromIntegral k) $ iterate (\x-> mod (x*x) n) b0
            iter []= False
            iter (x:xs) | x==1 = False
                    | x==n'= True
                    | otherwise = iter xs

