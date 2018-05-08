{- |
   Module      : Data.GraphViz.Types.GraphSpec
   Description : Testing graph-based graph representation
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.Types.GraphSpec (spec) where

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_findAllEdges, prop_findAllNodes,
                                         prop_findAllNodesE, prop_noGraphInfo,
                                         prop_printParseID)
import Data.GraphViz.Testing.Proxy      (DGProxy(..))
import Data.GraphViz.Types              (edgeInformation)
import Data.GraphViz.Types.Graph        (Context(..), DotEdge(..), DotGraph,
                                         DotNode(..), addEdge, emptyGraph,
                                         mkGraph, (&))

import Test.Hspec            (Spec, describe, it)
import Test.Hspec.QuickCheck (prop)

import Data.Graph.Inductive.PatriciaTree (Gr)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
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

  describe "issue#28" $ do
    it "mkGraph retains proper edge order" $
      hasEdge (mkGraph [DotNode 0 [], DotNode 1 []] [DotEdge 0 1 []]) (0,1)
    it "& retains proper edge order" $
      hasEdge (Cntxt { node = 1, inCluster = Nothing, attributes = [], predecessors = [(0,[])], successors = []}
               & Cntxt { node = 0, inCluster = Nothing, attributes = [], predecessors = [], successors = []}
               & emptyGraph)
              (0,1)
    it "addEdge retains proper edge order" $
      hasEdge (addEdge 0 1 [] (mkGraph [DotNode 0 [], DotNode 1 []] [])) (0,1)


dproxy :: DGProxy DotGraph
dproxy = DGProxy

hasEdge :: DotGraph Int -> (Int,Int) -> Bool
hasEdge dg (f,t) = edgeInformation False dg == [DotEdge f t []]
