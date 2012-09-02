import Euler.Helpers
data Fraction a = Numb a | Fraction (Fraction a) (Fraction a) deriving (Show, Eq)

numerator (Numb a) = a
numerator (Fraction (Numb a) (Numb b)) = a
numerator (Fraction (Numb a) (Fraction b c)) = a * denom (Fraction b c)
numerator (Fraction (Fraction a b) (Numb c)) = numerator (Fraction a b)
numerator (Fraction (Fraction a b) (Fraction c d)) = numerator (Fraction a b) * denom (Fraction c d)

denom (Numb a) = 1
denom (Fraction (Numb a) (Numb b)) = b
denom (Fraction (Fraction a b) (Numb c)) = c * denom (Fraction a b)
denom (Fraction (Numb a) (Fraction b c)) = numerator (Fraction b c) 
denom (Fraction (Fraction a b) (Fraction c d)) = denom (Fraction a b) * numerator (Fraction b c)

eval f = Fraction (Numb (n f)) (Numb (d f))
    where 
        n f = numerator f `div` gcd (numerator f) (denom f)
        d f = denom f `div` gcd (numerator f) (denom f)
pl a b = eval $ Fraction (Numb $ n a b + n b a) (Numb $ d a b)
    where
        n a b = numerator a * (d a b `div` denom a)
        d a b = lcm (denom a) (denom b)
sqrt2 n = Numb 1 `pl` Fraction (Numb 1) (sqrt2' n)
    where 
        sqrt2' 0 = Numb 2
        sqrt2' n = Numb 2 `pl` Fraction (Numb 1) (sqrt2' (n-1))        

problem58 = length $ filter (longer) (map sqrt2 [1..1000])
    where
        longer x = (digits $ numerator x) > (digits $ denom x)
        digits = length . toDigits
main = print $ problem58
