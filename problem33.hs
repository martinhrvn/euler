import Euler.Helpers
reduce :: (Integral a, Fractional b) => [a] -> [a] -> b
reduce (x1:x2:[]) (y1:y2:[])
    | x1 == y1 = d x2 y2
    | x1 == y2 = d x2 y1
    | x2 == y2 = d x1 y1
    | otherwise = 0
d x y = fromIntegral x / fromIntegral y
problem33 = foldr1 prod [(x,y) | x<-[99,98..11], y<-[99,98..x], x/=y, x `mod` 10 /= 0, y `mod` 10 /= 0, d x y == reduce (toDigits x) (toDigits y)]
    where prod x = (*) (fst x) Control.Arrow.*** (*) (snd x)

main = print problem33
