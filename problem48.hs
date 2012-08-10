import Euler.Helpers

limit n = fromDigits (reverse (take 10 (reverse (toDigits n))))

pow n = power n n

power n 0 = 1
power n m = limit (n*(power n (m-1)))

problem48 = sum (map pow [1..1000])

main = print problem48
