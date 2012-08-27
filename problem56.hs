import Euler.Helpers

sums = [digitalSum (x^y) | x <- [1..99], y <- [1..99]]
    where digitalSum n = sum $ toDigits n

main = print $ maximum sums
