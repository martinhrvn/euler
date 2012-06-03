main = print problem02
  
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
problem02 = sum (filter even (takeWhile (<4000000) fibs))
