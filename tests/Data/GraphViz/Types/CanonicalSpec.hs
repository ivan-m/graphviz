{- |
   Module      : Data.GraphViz.Types.CanonicalSpec
   Description : Testing canonical graph representation
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.Types.CanonicalSpec where

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties
import Data.GraphViz.Types.Canonical    (DotGraph)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)

import Data.Graph.Inductive.PatriciaTree (Gr)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  prop "Generalising a graph doesn't change Dot code"
       (prop_generalisedSameDot :: DotGraph Int -> Bool)
  prop "Printing and parsing Dot graph"
       (prop_printParseID :: DotGraph Int -> Bool)
