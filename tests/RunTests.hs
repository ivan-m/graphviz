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

import Data.GraphViz.Testing( Test(name, lookupName)
                            , defaultTests, runChosenTests)

import Data.Char(toLower)
import Data.Maybe(mapMaybe)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Arrow((&&&))
import Control.Monad(when)
import System.Environment(getArgs, getProgName)
import System.Exit(ExitCode(ExitSuccess), exitWith)

-- -----------------------------------------------------------------------------

main :: IO ()
main = do opts <- getArgs
          let opts' = map (map toLower) opts
              hasArg arg = any (arg==) opts'
          when (hasArg "help") helpMsg
          let tests = if hasArg "all"
                      then defaultTests
                      else mapMaybe getTest opts'
              tests' = if null tests
                       then defaultTests
                       else tests
          runChosenTests tests'

testLookup :: Map String Test
testLookup = Map.fromList
             $ map (lookupName &&& id) defaultTests

getTest :: String -> Maybe Test
getTest = (`Map.lookup` testLookup)

helpMsg :: IO ()
helpMsg = getProgName >>= (putStr . msg) >> exitWith ExitSuccess
  where
    msg nm = unlines
      [ "This utility is the test-suite for the graphviz library for Haskell."
      , "Various tests are available; see the table below for a complete list."
      , "There are several ways of running this program:"
      , ""
      , "    " ++ nm ++ "               Run all of the tests"
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
    testNames = map (andLen lookupName &&& andLen name) defaultTests
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
