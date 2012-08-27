import Euler.Helpers

lychrel x = not . any isPalindromeNum . take 50 . tail $ iterate next x
    where next n = n + (fromDigits . reverse $ toDigits n)
main = print . length $ filter lychrel [1..10000]
