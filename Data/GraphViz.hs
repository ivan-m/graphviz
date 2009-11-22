{- |
   Module      : Data.GraphViz
   Description : Graphviz bindings for Haskell.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the top-level module for the graphviz library.  It provides
   functions to convert 'Data.Graph.Inductive.Graph.Graph's into the
   /Dot/ language used by the /Graphviz/ suite of programs (as well as
   a limited ability to perform the reverse operation).

   Information about Graphviz and the Dot language can be found at:
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
      -- ** Utility functions
    , prettyPrint
    , prettyPrint'
      -- * Passing the graph through Graphviz.
      -- ** Type aliases for @Node@ and @Edge@ labels.
    , AttributeNode
    , AttributeEdge
      -- ** For normal graphs.
    , graphToGraph
    , graphToGraph'
    , dotizeGraph
    , dotizeGraph'
      -- ** For clustered graphs.
    , clusterGraphToGraph
    , clusterGraphToGraph'
    , dotizeClusterGraph
    , dotizeClusterGraph'
      -- * Re-exporting other modules.
    , module Data.GraphViz.Types
    , module Data.GraphViz.Attributes
    , module Data.GraphViz.Commands
    ) where

import Data.GraphViz.Types
import Data.GraphViz.Types.Clustering
import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.GraphViz.Types.Printing(PrintDot)

import Data.Graph.Inductive.Graph
import qualified Data.Set as Set
import Control.Arrow((&&&))
import Data.Maybe(mapMaybe, fromJust)
import qualified Data.Map as Map
import Control.Monad(liftM)
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

-- | Convert a graph to Graphviz's /Dot/ format.  The 'Bool' value is
--   'True' for directed graphs, 'False' otherwise.
graphToDot :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
              -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
              -> DotGraph Node
graphToDot isDir graph gAttributes
    = clusterGraphToDot isDir graph gAttributes clustBy cID fmtClust
      where
        clustBy :: LNode a -> NodeCluster () a
        clustBy = N
        cID = const Nothing
        fmtClust = const []

-- | Convert a graph to Graphviz's /Dot/ format with automatic
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
                     -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                     -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                     -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                     -> DotGraph Node
clusterGraphToDot dirGraph graph gAttrs clusterBy cID fmtCluster fmtNode fmtEdge
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
        (cs, ns) = clustersToNodes clusterBy cID fmtCluster fmtNode graph
        es = mapMaybe mkDotEdge . labEdges $ graph
        mkDotEdge e@(f,t,_) = if dirGraph || f <= t
                              then Just DotEdge { edgeFromNodeID = f
                                                , edgeToNodeID   = t
                                                , edgeAttributes = fmtEdge e
                                                , directedEdge   = dirGraph
                                                }
                              else Nothing

-- | Convert a graph to /Dot/ format, using the specified clustering function
--   to group nodes into clusters.
--   Clusters can be nested to arbitrary depth.
--   Graph direction is automatically inferred.
clusterGraphToDot'    :: (Ord c, Ord b, Graph gr) => gr a b
                         -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                         -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                         -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                         -> DotGraph Node
clusterGraphToDot' gr = clusterGraphToDot (isDirected gr) gr

-- -----------------------------------------------------------------------------

type AttributeNode a = (Attributes, a)
type AttributeEdge b = (Attributes, b)

-- | Run the appropriate Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.  Note that for the edge information to
--   be parsed properly when using multiple edges, each edge between
--   two nodes needs to have a unique label.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
graphToGraph :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
                -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph isDir gr gAttributes fmtNode fmtEdge
    = dotAttributes isDir gr dot
    where
      dot = graphToDot isDir gr gAttributes fmtNode fmtEdge

dotAttributes :: (Graph gr) => Bool -> gr a b -> DotGraph Node
                 -> IO (gr (AttributeNode a) (AttributeEdge b))
dotAttributes isDir gr dot
    = do (Right output) <- graphvizWithHandle command
                                              dot
                                              DotOutput
                                              hGetContents'
         return $ rebuildGraphWithAttributes output
    where
      command = if isDir then dirCommand else undirCommand
      rebuildGraphWithAttributes dotResult =  mkGraph lnodes ledges
          where
            lnodes = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
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
            edgeMap = Map.fromList $ map ( (edgeFromNodeID &&& edgeToNodeID)
                                           &&& edgeAttributes) es

-- | Run the appropriate Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.
--
--   Graph direction is automatically inferred.
graphToGraph'    :: (Ord b, Graph gr) => gr a b -> [GlobalAttributes]
                    -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                    -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph' gr = graphToGraph (isDirected gr) gr

-- | Run the appropriate Graphviz command on the clustered graph to
--   get positional information and then combine that information back
--   into the original graph.  Note that for the edge information to
--   be parsed properly when using multiple edges, each edge between
--   two nodes needs to have a unique label.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
clusterGraphToGraph :: (Ord c, Graph gr) => Bool -> gr a b
                       -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                       -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                       -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                       -> IO (gr (AttributeNode a) (AttributeEdge b))
clusterGraphToGraph isDir gr gAtts clBy cID fmtClust fmtNode fmtEdge
    = dotAttributes isDir gr dot
    where
      dot = clusterGraphToDot isDir gr gAtts clBy cID fmtClust fmtNode fmtEdge

-- | Run the appropriate Graphviz command on the clustered graph to
--   get positional information and then combine that information back
--   into the original graph.
--
--   Graph direction is automatically inferred.
clusterGraphToGraph'    :: (Ord b, Ord c, Graph gr) => gr a b
                           -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                           -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                           -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                           -> IO (gr (AttributeNode a) (AttributeEdge b))
clusterGraphToGraph' gr = clusterGraphToGraph (isDirected gr) gr


-- | Pass the graph through 'graphToGraph' with no 'Attribute's.  This
--   is an @'IO'@ action, however since the state doesn't change it's
--   safe to use 'unsafePerformIO' to convert this to a normal
--   function.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
dotizeGraph         :: (Graph gr) => Bool -> gr a b
                       -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph isDir g = unsafePerformIO
                      $ graphToGraph isDir g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []

-- | Pass the graph through 'graphToGraph' with no 'Attribute's.  This
--   is an @'IO'@ action, however since the state doesn't change it's
--   safe to use 'unsafePerformIO' to convert this to a normal
--   function.
--
--   The graph direction is automatically inferred.
dotizeGraph'   :: (Graph gr, Ord b) => gr a b
                  -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph' g = dotizeGraph (isDirected g) g

-- | Pass the clustered graph through 'clusterGraphToGraph' with no
--   'Attribute's.  This is an @'IO'@ action, however since the state
--   doesn't change it's safe to use 'unsafePerformIO' to convert this
--   to a normal function.
--
--   The 'Bool' argument is 'True' for directed graphs, 'False'
--   otherwise.  Directed graphs are passed through /dot/, and
--   undirected graphs through /neato/.
dotizeClusterGraph                 :: (Ord c, Graph gr) => Bool -> gr a b
                                      -> (LNode a -> NodeCluster c l)
                                      -> gr (AttributeNode a) (AttributeEdge b)
dotizeClusterGraph isDir g clustBy = unsafePerformIO
                                     $ clusterGraphToGraph isDir   g
                                                           gAttrs  clustBy
                                                           cID     cAttrs
                                                           noAttrs noAttrs
    where
      gAttrs = []
      cID = const Nothing
      cAttrs = const gAttrs
      noAttrs = const []

-- | Pass the clustered graph through 'graphToGraph' with no
--   'Attribute's.  This is an @'IO'@ action, however since the state
--   doesn't change it's safe to use 'unsafePerformIO' to convert this
--   to a normal function.
--
--   The graph direction is automatically inferred.
dotizeClusterGraph'   :: (Ord b, Ord c, Graph gr) => gr a b
                         -> (LNode a -> NodeCluster c l)
                         -> gr (AttributeNode a) (AttributeEdge b)
dotizeClusterGraph' g = dotizeClusterGraph (isDirected g) g

-- -----------------------------------------------------------------------------
-- Utility Functions

-- | Pretty-print the 'DotGraph' by passing it through the 'Canon'
--   output type (which produces \"canonical\" output).  This is
--   required because the @printIt@ function in
--   "Data.GraphViz.Types.Printing" no longer uses indentation to
--   ensure the Dot code is printed correctly.
prettyPrint    :: (PrintDot a) => DotGraph a -> IO String
prettyPrint dg = liftM fromRight
                 -- Note that the choice of command here should be
                 -- arbitrary.
                 $ graphvizWithHandle (commandFor dg)
                                      dg
                                      Canon
                                      hGetContents'
  where
    fromRight (Right r) = r
    fromRight Left{}    = fail "Usage of prettyPrint failed; \
                                \is the Graphviz suite of tools installed?"

-- | The 'unsafePerformIO'd version of 'prettyPrint'.  Graphviz should
--   always produce the same pretty-printed output, so this should be
--   safe.
prettyPrint' :: (PrintDot a) => DotGraph a -> String
prettyPrint' = unsafePerformIO . prettyPrint
