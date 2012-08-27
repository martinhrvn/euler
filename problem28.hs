import Euler.Helpers

diagonal n = take (2*n-1) diagonals

main = print . sum $ diagonal 1001
