{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix, sort)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Data.Ord
import Cards
import System.Random
import Hands

-- Simple function to create a hello message.
hello s = "Hello " ++ s
h1 = [Card Two Clubs, Card Two Diamonds, Card Two Hearts, Card Eight Diamonds, Card Nine Spades]
-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s
prop_sm = evaluateHand h1 == Hand ThreeOfAKind [Two, Nine, Eight]

-- Hello World
exeMain = do
    gen <- getStdGen
    print $ take 10 (shuffle gen deck)
    print . sort . fst $ fisherYates gen deck
    --print $ filterCards h1 1
    where

        h2 = evaluateHand $ [Card Two Clubs, Card Two Diamonds, Card Two Hearts, Card Two Diamonds, Card Nine Spades]
-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

