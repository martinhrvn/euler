import Euler.Helpers
irrat n = last (map irrational [1..n] ) !! (n-1)
irrational 0 = []
irrational n = (irrational (n-1))++(toDigits n)

main = print (foldr1 (*) (map irrat (reverse [1,10,100,1000,10000,100000,1000000]) ))
