{- |
   Module      : Data.GraphViz
   Description : GraphViz bindings for Haskell.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the top-level module for the graphviz library.  It provides
   functions to convert 'Data.Graph.Inductive.Graph.Graph's into the
   /Dot/ language used by the /GraphViz/ suite of programs (as well as
   a limited ability to perform the reverse operation).

   Information about GraphViz and the Dot language can be found at:
   <http://graphviz.org/>

   Commands for converting graphs to Dot format have two options: one
   in which the user specifies whether the graph is directed or
   undirected, and a primed version which attempts to automatically
   infer if the graph is directed or not.  Note that these conversion
   functions assume that undirected graphs have every edge being
   duplicated (or at least that if there exists an edge from /n1/ to
   /n2/, then /n1 <= n2/).
 -}

module Data.GraphViz
    ( -- * Conversion from graphs to /Dot/ format.
      graphToDot
    , graphToDot'
      -- ** Conversion with support for clusters.
    , NodeCluster(..)
    , clusterGraphToDot
    , clusterGraphToDot'
      -- * Passing the graph through GraphViz.
    , AttributeNode
    , AttributeEdge
    , graphToGraph
    , graphToGraph'
    , dotizeGraph
    , dotizeGraph'
      -- * Re-exporting other modules.
    , module Data.GraphViz.Types
    , module Data.GraphViz.Attributes
    , module Data.GraphViz.Commands
    ) where

import Data.GraphViz.Types
import Data.GraphViz.Types.Clustering
import Data.GraphViz.Attributes
import Data.GraphViz.Commands

import Data.Graph.Inductive.Graph
import qualified Data.Set as Set
import Control.Arrow((&&&))
import Data.Maybe(mapMaybe, fromJust)
import qualified Data.Map as Map
import System.IO(hGetContents)
import System.IO.Unsafe(unsafePerformIO)

-- -----------------------------------------------------------------------------

-- | Determine if the given graph is undirected.
isUndirected   :: (Ord b, Graph g) => g a b -> Bool
isUndirected g = all hasFlip es
    where
      es = labEdges g
      eSet = Set.fromList es
      hasFlip e = Set.member (flippedEdge e) eSet
      flippedEdge (f,t,l) = (t,f,l)

-- | Determine if the given graph is directed.
isDirected :: (Ord b, Graph g) => g a b -> Bool
isDirected = not . isUndirected

-- -----------------------------------------------------------------------------

-- | Convert a graph to GraphViz's /Dot/ format.  The 'Bool' value is
--   'True' for directed graphs, 'False' otherwise.
graphToDot :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
              -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
              -> DotGraph Node
graphToDot isDir graph gAttributes fmtNode fmtEdge
    = clusterGraphToDot isDir graph gAttributes clusterBy fmtCluster fmtNode fmtEdge
      where
        clusterBy :: LNode a -> NodeCluster () a
        clusterBy = N
        fmtCluster _ = []

-- | Convert a graph to GraphViz's /Dot/ format with automatic
--   direction detection.
graphToDot'       :: (Ord b, Graph gr) => gr a b -> [GlobalAttributes]
                     -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                     -> DotGraph Node
graphToDot' graph = graphToDot (isDirected graph) graph

-- | Convert a graph to /Dot/ format, using the specified clustering function
--   to group nodes into clusters.
--   Clusters can be nested to arbitrary depth.
--   The 'Bool' argument is 'True' for directed graphs, 'False' otherwise.
clusterGraphToDot :: (Ord c, Graph gr) => Bool -> gr a b
                     -> [GlobalAttributes] -> (LNode a -> NodeCluster c a)
                     -> (c -> [GlobalAttributes]) -> (LNode a -> Attributes)
                     -> (LEdge b -> Attributes) -> DotGraph Node
clusterGraphToDot dirGraph graph gAttrs clusterBy fmtCluster fmtNode fmtEdge
    = DotGraph { strictGraph     = False
               , directedGraph   = dirGraph
               , graphID         = Nothing
               , graphStatements = stmts
               }
      where
        stmts = DotStmts { attrStmts = gAttrs
                         , subGraphs = cs
                         , nodeStmts = ns
                         , edgeStmts = es
                         }
        (cs, ns) = clustersToNodes clusterBy fmtCluster fmtNode graph
        es = mapMaybe mkDotEdge . labEdges $ graph
        mkDotEdge e@(f,t,_) = if dirGraph || f <= t
                              then Just DotEdge { edgeHeadNodeID = f
                                                , edgeTailNodeID = t
                                                , edgeAttributes = fmtEdge e
                                                , directedEdge   = dirGraph
                                                }
                              else Nothing

-- | Convert a graph to /Dot/ format, using the specified clustering function
--   to group nodes into clusters.
--   Clusters can be nested to arbitrary depth.
--   Graph direction is automatically inferred.
clusterGraphToDot'       :: (Ord c, Ord b, Graph gr) => gr a b
                            -> [GlobalAttributes] -> (LNode a -> NodeCluster c a)
                            -> (c -> [GlobalAttributes]) -> (LNode a -> Attributes)
                            -> (LEdge b -> Attributes) -> DotGraph Node
clusterGraphToDot' graph = clusterGraphToDot (isDirected graph) graph

-- -----------------------------------------------------------------------------

type AttributeNode a = (Attributes, a)
type AttributeEdge b = (Attributes, b)

-- | Run the graph via dot to get positional information and then
--   combine that information back into the original graph.
--   Note that this doesn't support graphs with clusters.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
graphToGraph :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
                -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph isDir gr gAttributes fmtNode fmtEdge
    = do output <- graphvizWithHandle command dot DotOutput hGetContents
         let res = fromJust output
         length res `seq` return ()
         return $ rebuildGraphWithAttributes res
    where
      command = if isDir then dirCommand else undirCommand
      dot = graphToDot isDir gr gAttributes fmtNode fmtEdge
      rebuildGraphWithAttributes dotResult = mkGraph lnodes ledges
          where
            lnodes = map (\(n, l) -> (n, (fromJust $ Map.lookup n nodeMap, l)))
                     $ labNodes gr
            ledges = map createEdges $ labEdges gr
            createEdges (f, t, l) = if isDir || f <= t
                                    then (f, t, getLabel (f,t))
                                    else (f, t, getLabel (t,f))
                where
                  getLabel c = (fromJust $ Map.lookup c edgeMap, l)
            g' = parseDotGraph dotResult
            ns = graphNodes g'
            es = graphEdges g'
            nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
            edgeMap = Map.fromList $ map ( (edgeHeadNodeID &&& edgeTailNodeID)
                                           &&& edgeAttributes) es

-- | Run the graph via dot to get positional information and then
--   combine that information back into the original graph.
--   Note that this doesn't support graphs with clusters.
--   Graph direction is automatically inferred.
graphToGraph'    :: (Ord b, Graph gr) => gr a b -> [GlobalAttributes]
                    -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                    -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph' gr = graphToGraph (isDirected gr) gr

-- | Pass the plain graph through 'graphToGraph'.  This is an @'IO'@ action,
--   however since the state doesn't change it's safe to use 'unsafePerformIO'
--   to convert this to a normal function.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
dotizeGraph         :: (DynGraph gr) => Bool -> gr a b
                       -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph isDir g = unsafePerformIO
                      $ graphToGraph isDir g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []

-- | Pass the plain graph through 'graphToGraph'.  This is an @'IO'@ action,
--   however since the state doesn't change it's safe to use 'unsafePerformIO'
--   to convert this to a normal function.
--   The graph direction is automatically inferred.
dotizeGraph'   :: (DynGraph gr, Ord b) => gr a b
                  -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph' g = dotizeGraph (isDirected g) g
