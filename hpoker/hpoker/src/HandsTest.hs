-----------------------------------------------------------------------------
--
-- Module      :  HandsTest
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

module HandsTest where
import Cards
import Hands
import qualified Data.List as L
import Test.QuickCheck
import Text.Printf
import Data.Function (on)

instance Arbitrary Card where
   arbitrary =  do
      rank <- elements Cards.ranks
      suit <- elements suits
      return (Card rank suit)

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

noHand :: Gen [Card]
noHand = do
    rankA <- elements [Two .. Four]
    suitA <- elements [Diamonds, Spades]
    rankB <- elements [Five .. Eight]
    suitB <- elements suits
    rankC <- elements [Nine .. Ten]
    suitC <- elements [Diamonds, Spades]
    rankD <- elements [Jack .. Queen]
    suitD <- elements [Diamonds, Spades]
    rankE <- elements [King .. Ace]
    suitE <- elements [Clubs, Hearts]
    return [(Card rankA suitA),(Card rankB suitB),(Card rankC suitC),(Card rankD suitD),(Card rankE suitE)]


prop_nohand = forAll noHand $ \xs -> htype (evaluateHand xs) == NoHand

tests  = [("nohand", deepCheck prop_nohand)]

