import Data.Bits
import Data.Char
import Data.List (delete)
import Data.List.Split

legalChars = map ord (['A'..'Z']++['a'..'z']++" .,!?(;)'1234567890")
lc = map ord ['a'..'z']
passwords = map cycle [[a,b,c] | a <- lc, b<-lc, c <- lc]
bruteforce xs = map sum (filter allLegalChars (map (zipWith xor xs) passwords))
    where
        allLegalChars xs = all (`elem` legalChars) xs
preprocess s = map (\x -> read x :: Int) (splitOn "," (delete '\n' s)) 
main = do
    contents <- readFile "cipher1.txt"
    print . bruteforce $ preprocess contents 
