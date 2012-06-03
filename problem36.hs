import Euler.Helpers

decToBin 0 = []
decToBin n = (n `mod` 2):(decToBin (n `div` 2) )

problem36 = [x | x<-[1..1000000], isPalindrome (toDigits x), isPalindrome (decToBin x)]

main = print . sum $ problem36



