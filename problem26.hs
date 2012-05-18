import Data.List as L
divide n a 
   | n `mod` a == 0 = (n `div` a, n `mod` a):[]
   | n < a = (n `div` a, n `mod` a):(divide (n*10) a)
   | otherwise = (n `div` a, n `mod` a):(divide ((n `mod` a)*10) a)

tWhile [] _ = 0
tWhile (x:xs) ys
    | (snd x) == 0 = - (length ys)
    | elem x ys = (-(head(elemIndices x (reverse ys))))
    | otherwise = 1 + tWhile xs (x:ys)
problem26 = maximum [(tWhile (divide 1 x) [], x) | x<-[1..1000]]
main = print (problem26)
