isPalindrome n = n == reverse n

toArray  0 = []
toArray  n = lastDigit n : toArray (div n 10)
    where lastDigit n = n - div n 10 * 10
        
problem10 = maximum [x*y | x<-[999,998..1], y<-[999,998..1], isPalindrome (toArray (x*y))]
main = print problem10
