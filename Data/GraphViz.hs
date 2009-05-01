{-# LANGUAGE RecordPuns
           , ScopedTypeVariables
           #-}

{- |
   Module      : Data.GraphViz
   Description : GraphViz bindings for Haskell.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the top-level module for the graphviz library.  It provides
   functions to create graphs using the /Dot/ language for the
   /GraphViz/ program:

       <http://graphviz.org/>

   It also has limited ability to parse graphs in the Dot language.

   Note that this module re-exports the "data.GraphViz.Attributes"
   module, which exports a constructor that clashes with
   'Prelude.LT'.  As such, you may need to import either this module
   or the "Prelude" qualified or hiding @LT@.

   -}

module Data.GraphViz
    ( graphToDot
    , clusterGraphToDot
    , graphToGraph
    , dotizeGraph
    , NodeCluster(..)
    , AttributeNode
    , AttributeEdge
    , module Data.GraphViz.Types
    , module Data.GraphViz.Attributes
    , module Data.GraphViz.Commands
    )
    where

import Data.Graph.Inductive.Graph
import Data.List
import Data.Function
import qualified Data.Set as Set
import Text.ParserCombinators.Poly.Lazy
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import System.IO
import System.IO.Unsafe(unsafePerformIO)

import Data.GraphViz.Types
import Data.GraphViz.Types.Clustering
import Data.GraphViz.Attributes
import Data.GraphViz.Commands

-- -----------------------------------------------------------------------------

-- | Determine if the given graph is undirected or directed.
isUndir   :: (Ord b, Graph g) => g a b -> Bool
isUndir g = all hasFlip edges
    where
      edges = labEdges g
      eSet = Set.fromList edges
      hasFlip e = Set.member (flippedEdge e) eSet
      flippedEdge (f,t,l) = (t,f,l)

-- -----------------------------------------------------------------------------

-- | Convert a graph to /Dot/ format.
graphToDot :: (Ord b, Graph gr) => gr a b -> [Attribute]
           -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> DotGraph
graphToDot graph graphAttributes fmtNode fmtEdge
    = clusterGraphToDot graph graphAttributes clusterBy fmtCluster fmtNode fmtEdge
      where
        clusterBy :: LNode a -> NodeCluster () a
        clusterBy = N
        fmtCluster _ = []

-- | Convert a graph to /Dot/ format, using the specified clustering function
--   to group nodes into clusters.
--   Clusters can be nested to arbitrary depth.
clusterGraphToDot :: (Ord c, Ord b, Graph gr) => gr a b
                  -> [Attribute] -> (LNode a -> NodeCluster c a)
                  -> (c -> [Attribute]) -> (LNode a -> [Attribute])
                  -> (LEdge b -> [Attribute]) -> DotGraph
clusterGraphToDot graph graphAttributes clusterBy fmtCluster fmtNode fmtEdge
    = DotGraph { graphAttributes, graphNodes, graphEdges, directedGraph }
      where
        graphNodes = clustersToNodes clusterBy fmtCluster fmtNode graph
        directedGraph = not $ isUndir graph
        graphEdges = catMaybes . map mkDotEdge . labEdges $ graph
        mkDotEdge e@(f,t,_) = if (directedGraph || f <= t)
                              then Just $ DotEdge {edgeHeadNodeID = t
                                                  ,edgeTailNodeID = f
                                                  ,edgeAttributes = fmtEdge e
                                                  ,directedEdge = directedGraph}
                              else Nothing

type AttributeNode a = ([Attribute], a)
type AttributeEdge b = ([Attribute], b)

-- | Run the graph via dot to get positional information and then
--   combine that information back into the original graph.
--   Note that this doesn't support graphs with clusters.
graphToGraph :: forall gr a b . (Ord b, Graph gr) =>
                gr a b -> [Attribute] -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph gr graphAttributes fmtNode fmtEdge
    = do { out <- graphvizWithHandle command dot DotOutput hGetContents
         ; let res = fromJust out
         ; (length res) `seq` return ()
         ; return $ rebuildGraphWithAttributes res
         }
    where
      undirected = isUndir gr
      command = if undirected then undirCommand else dirCommand
      dot = graphToDot gr graphAttributes fmtNode fmtEdge
      rebuildGraphWithAttributes :: String -> gr (AttributeNode a) (AttributeEdge b)
      rebuildGraphWithAttributes dotResult = mkGraph lnodes ledges
          where
            lnodes = map (\(n, l) -> (n, (fromJust $ Map.lookup n nodeMap, l))) . labNodes $ gr
            ledges = map createEdges . labEdges $ gr
            (DotGraph { graphEdges, graphNodes }) = fst . runParser readDotGraph $ dotResult
            nodeMap = Map.fromList . map (\n -> (nodeID n, nodeAttributes n)) $ graphNodes
            edgeMap = Map.fromList . map (\e -> ((edgeTailNodeID e, edgeHeadNodeID e), edgeAttributes e)) $ graphEdges
            createEdges (f,t,l) = if (undirected && f > t)
                                  then (f,t,getLabel (t,f))
                                  else (f,t,getLabel (f,t))
                where
                  getLabel c = (fromJust $ Map.lookup c edgeMap,l)

-- | Pass the plain graph through 'graphToGraph'.  This is an IO action,
--   however since the state doesn't change it's safe to use 'unsafePerformIO'
--   to convert this to a normal function.
dotizeGraph   :: (DynGraph gr, Ord b) => gr a b
              -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph g = unsafePerformIO
                $ graphToGraph g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []
