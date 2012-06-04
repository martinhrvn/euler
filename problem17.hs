import Euler.Helpers
digits = ["", "one","two","three","four","five","six","seven","eight"
         ,"nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen"
         ,"seventeen", "eighteen", "nineteen"]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

spell n
    | n < 20 = digits !! n
    | n == 1000 = "onethousand"
    | n < 100 = tens !! ten n ++ (digits !! digit n)
    | n `mod` 100 == 0 = digits !! (n `div` 100) ++ "hundred"
    | n < 1000 = digits !! hundred n ++ "hundredand" ++ spell (n `mod` 100)
    where 
        digit n = last (toDigits n)
        ten n = reverse (toDigits n) !! 1
        hundred n = reverse (toDigits n) !! 2

main = print ( length (foldr1 (++) (map spell [1..1000]))) 
