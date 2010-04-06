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

import Data.GraphViz.Testing(runDefaultTests)

main :: IO ()
main = runDefaultTests
