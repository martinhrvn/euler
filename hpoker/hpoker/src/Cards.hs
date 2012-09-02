
module Cards where
import Control.Applicative
import Data.List (concat)
import System.Random
import Data.Map hiding (foldl)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)

data Card = BlankCard | Card {rank :: Rank, suit :: Suit}
    deriving (Eq, Ord, Show)


suits = [Clubs, Diamonds, Hearts, Spades]
ranks = [Two .. Ace]

deck :: [Card]
deck = Card <$> ranks <*> suits

decks n = concat $ replicate n deck

shuffle g xs = fst (fisherYates g xs) : shuffle (snd (fisherYates g xs)) xs

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)
