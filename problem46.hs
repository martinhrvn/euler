import Euler.Helpers

oddComposites = [x | x <- [3,5..], not . isPrime $ x]

existsSum x = null [y | y <- takeWhile (<x) primes, z <- [1..sqr ((x-y) `div` 2)], x == y + 2*(z*z)]

problem46 = head [x | x <- oddComposites, existsSum x]

main = print problem46
