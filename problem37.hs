import Euler.Helpers

isTruncatableFromLeft [] = True
isTruncatableFromLeft (x:xs) = isPrime ( fromDigits ( reverse (x:xs))) && isTruncatableFromLeft xs
isTruncatebleFromRight [] = True
isTruncatebleFromRight (x:xs) = isPrime (fromDigits (x:xs)) && isTruncatebleFromRight xs

problem37 = take 11 [x | x<-primes, x > 7, head (toDigits x) /= 1, head (reverse (toDigits x)) /= 1, isTruncatableFromLeft (reverse (toDigits x)), isTruncatebleFromRight (toDigits x)]

main = print . sum $ problem37
