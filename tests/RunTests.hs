{- |
   Module      : RunTests
   Description : Run the graphviz test suite.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module exists solely to make a Main module to build and run
   the test suite.
-}
module Main where

import Data.GraphViz.Testing (Test (name, lookupName), allTests, defaultTests,
                              runChosenTests)

import           Control.Arrow      ((&&&))
import           Control.Monad      (when)
import           Data.Char          (toLower)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (mapMaybe)
import           System.Environment (getArgs, getProgName)
import           System.Exit        (exitSuccess)

-- -----------------------------------------------------------------------------

main :: IO ()
main = do opts <- getArgs
          let opts' = map (map toLower) opts
              hasArg arg = arg `elem` opts'
          when (hasArg "help") helpMsg
          let tests = if hasArg "all"
                      then allTests
                      else mapMaybe getTest opts'
              tests' = if null tests
                       then defaultTests
                       else tests
          runChosenTests tests'

testLookup :: Map String Test
testLookup = Map.fromList
             $ map (lookupName &&& id) allTests

getTest :: String -> Maybe Test
getTest = (`Map.lookup` testLookup)

helpMsg :: IO ()
helpMsg = getProgName >>= (putStr . msg) >> exitSuccess
  where
    msg nm = unlines
      [ "This utility is the test-suite for the graphviz library for Haskell."
      , "Various tests are available; see the table below for a complete list."
      , "There are several ways of running this program:"
      , ""
      , "    " ++ nm ++ "               Run the default set of tests"
      , "    " ++ nm ++ " all           Run all of the tests"
      , "    " ++ nm ++ " help          Get this help message"
      , "    " ++ nm ++ " <key>         Run the test associated with each key,"
      , "        (where <key> denotes a space-separated list of keys"
      , "         from the table below)."
      , ""
      , helpTable
      ]

helpTable :: String
helpTable = unlines $ fmtName ((lnHeader,lnHeaderLen),(nHeader,nHeaderLen))
                      : line
                      : map fmtName testNames
  where
    andLen = ((id &&& length) .)
    testNames = map (andLen lookupName &&& andLen name) allTests
    fmtName ((ln,lnl),(n,_)) = concat [ ln
                                      , replicate (maxLN-lnl+spacerLen) ' '
                                      , n
                                      ]
    line = replicate (maxLN + spacerLen + maxN) '-'
    maxLN = maximum $ map (snd . fst) testNames
    maxN = maximum $ map (snd . snd) testNames
    spacerLen = 3
    lnHeader = "Key"
    lnHeaderLen = length lnHeader
    nHeader = "Description"
    nHeaderLen = length nHeader
