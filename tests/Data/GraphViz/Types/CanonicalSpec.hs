{- |
   Module      : Data.GraphViz.Types.CanonicalSpec
   Description : Testing canonical graph representation
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.Types.CanonicalSpec (spec) where

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_findAllEdges, prop_findAllNodes,
                                         prop_findAllNodesE,
                                         prop_generalisedSameDot,
                                         prop_noGraphInfo, prop_printParseID)
import Data.GraphViz.Testing.Proxy      (DGProxy(..))
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
  prop "Find all nodes in a Dot graph"
       (prop_findAllNodes dproxy :: Gr () () -> Bool)
  prop "Find all nodes in an node-less Dot graph"
       (prop_findAllNodesE dproxy :: Gr () () -> Bool)
  prop "Find all edges in a Dot graph"
       (prop_findAllEdges dproxy :: Gr () () -> Bool)
  prop "Plain Dot graphs should have no structural information"
       (prop_noGraphInfo dproxy :: Gr () () -> Bool)

dproxy :: DGProxy DotGraph
dproxy = DGProxy
