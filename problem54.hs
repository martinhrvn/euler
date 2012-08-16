import qualified Data.List as L
import Control.Monad
import System.IO
parseHand s = map L.sort ([take 5 (processHand s)]++[drop 5 (processHand s)])
    where
        processHand s = (map processCards (words s))
        processCards c = (val (take 1 c), drop 1 c)
        val "T" = 10
        val "J" = 11
        val "Q" = 12
        val "K" = 13
        val "A" = 14
        val s = read s

evaluateHand h
    | isRoyalFlush h = 9000
    | isStraightFlush h = 8000 + fst (head h)
    | isFourOfAKind h = 7000 + fokValue h
    | isFullHouse h = 6000 + fullHouseValue h
    | isFlush h = 5000 + fst (last h)
    | isStraight h = 4000 + fst (head h)
    | isThreeOfKind h = 3000 + tokValue h
    | isTwoPairs h = 2000 + twoPairsValue h
    | isPair h = 1000 + pairValue h 
    | otherwise = fst (last h)
    where
        isPair h = cont h 2 2
        pairValue h = val h 2
        isThreeOfKind h = cont h 3 3
        tokValue h = val h 3
        isTwoPairs h = cont h 2 4
        isStraight h = and $ zipWith (\x y -> y == succ x) (extractVal h) (tail $ extractVal h)
        twoPairsValue h = fst (last (L.sort (filterCards h 2))) * 20 + fst (head (L.sort (filterCards h 2)))
        extractVal h = map fst h
        extractSuit h = map snd h
        isFlush h = 1 == (length . L.group $ extractSuit h)
        isFullHouse h = length (filterCards h 3) == 3 && length (filterCards h 2) == 2
        fullHouseValue h = val h 3
        isFourOfAKind h = cont h 4 4 
        fokValue h = val h 4
        isStraightFlush h = isStraight h && isFlush h
        isRoyalFlush h = isStraightFlush h && fst (last h) == 14
        val h n = fst . head $ filterCards h n
        cont h n a = length (filterCards h n) == a
--    | isRoyalFlush h = 1000
--    | isStraightFlush h = 900 + value (fst (head h))
--    | isFourOfAKind h = 800 + value(fst (head h))
--    | isFullHouse h = 700 + value(fst (head h))

filterCards h n = concat (filter (\x -> length x == n) (groupCards h))

groupCards h = L.groupBy (\x y -> fst x == fst y) h
scores h = map evaluateHand h
wins h = length (filter (\x -> head x > last x) h )
main = do
    contents <- readFile "poker.txt"
    print $ wins (map scores (map parseHand (lines contents)))
