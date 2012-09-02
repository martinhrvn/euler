-----------------------------------------------------------------------------
--
-- Module      :  Hands
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Hands where
import Cards
import qualified Data.List as L
import Data.Function (on)
import Control.Applicative
data CardHand = CardHand Card Card Card Card Card deriving (Show)
data Hand = Hand {htype :: HandType, ranks :: [Rank]} deriving (Eq, Ord, Show)

data HandType =
    NoHand |
    Pair |
    TwoPairs |
    ThreeOfAKind |
    Straight |
    Flush |
    FullHouse |
    FourOfAKind |
    StraightFlush |
    RoyalFlush
        deriving (Show, Eq, Ord, Enum)


fromCardHand (CardHand a b c d e) = [a,b,c,d,e]
toCardHand (a:b:c:d:e:[]) = CardHand a b c d e

evaluateHand :: [Card] -> Hand
evaluateHand h = evalHand (L.sort h)
    where
    evalHand h
        | isRoyalFlush h = Hand RoyalFlush [Ace]
        | isStraightFlush h = Hand StraightFlush [rank $ head h]
        | isFourOfAKind h = Hand FourOfAKind (val h 4)
        | isFullHouse h = Hand FullHouse (val h 3 ++ val h 2)
        | isFlush h = Hand Flush (map rank (reverse h))
        | isStraight h = Hand Straight [rank $ head h]
        | isThreeOfKind h = Hand ThreeOfAKind (val h 3)
        | isTwoPairs h = Hand TwoPairs (twoPairsValue h ++ L.sort (vals h 1))
        | isPair h = Hand Pair  (val h 2)
        | otherwise = Hand NoHand (map rank (reverse h))
        where
            isPair h = cont h 2 2
            isThreeOfKind h = cont h 3 3
            isTwoPairs h = cont h 2 4
            isStraight h = and $ zipWith (\x y -> y == succ x) (extractVal h) (tail $ extractVal h)
            twoPairsValue h = [rank (last (L.sort (filterCards h 2))) , rank (head (L.sort (filterCards h 2))) ]
            isFlush h = 1 == (length . L.group $ extractSuit h)
            isFullHouse h = length (filterCards h 3) == 3 && length (filterCards h 2) == 2
            isFourOfAKind h = cont h 4 4
            isStraightFlush h = isStraight h && isFlush h
            isRoyalFlush h = isStraightFlush h && rank (last h) == Ace
            val h n = (rank . head $ filterCards h n) : reverse (L.sort $ vals h 1)
            vals h n = map rank (filterCards h n)
            cont h n a = length (filterCards h n) == a

extractVal :: [Card] -> [Rank]
extractVal = map rank

extractSuit :: [Card] -> [Suit]
extractSuit = map suit


filterCards h n = concat (filter ((==n) . length) (groupCards rank h))

groupCards f = L.groupBy ((==) `on` f)
