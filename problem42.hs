import Data.Maybe
import qualified Data.List as L
import Data.List.Split
triangles = map trian [1..]
    where
        trian n = n*(n+1) `div` 2
isTriangle n = n `elem` takeWhile (<=n) triangles
isTriangleWord s = isTriangle . sum $ map charValue s
    where charValue c = fromJust ( L.elemIndex c ['A'..'Z']) + 1

main = do 
    contents <- readFile "words.txt"
    print . length $ filter isTriangleWord (map (filter (/= '"')) (splitOn "," contents))
