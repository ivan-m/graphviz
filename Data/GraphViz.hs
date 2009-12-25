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

 -}
module Data.GraphViz
    ( -- * Conversion from graphs to /Dot/ format.
      -- $conversion
      graphToDot
    , graphToDot'
      -- ** Conversion with support for clusters.
    , NodeCluster(..)
    , clusterGraphToDot
    , clusterGraphToDot'
      -- ** Pseudo-inverse conversion.
    , dotToGraph
      -- * Graph augmentation.
      -- $augment
      -- ** Type aliases for @Node@ and @Edge@ labels.
    , AttributeNode
    , AttributeEdge
      -- ** Customisable augmentation.
    , graphToGraph
    , graphToGraph'
    , clusterGraphToGraph
    , clusterGraphToGraph'
      -- ** Quick augmentation.
      -- $quickAugment
    , dotizeGraph
    , dotizeGraph'
    , dotizeClusterGraph
    , dotizeClusterGraph'
      -- ** Manual augmentation.
      -- $manualAugment
    , EdgeID
    , addEdgeIDs
    , setEdgeComment
    , dotAttributes
    , augmentGraph
      -- * Utility functions
    , prettyPrint
    , prettyPrint'
    , preview
      -- * Re-exporting other modules.
    , module Data.GraphViz.Types
    , module Data.GraphViz.Attributes
    , module Data.GraphViz.Commands
    ) where

import Data.GraphViz.Types
import Data.GraphViz.Types.Clustering
import Data.GraphViz.Util(uniq, uniqBy)
import Data.GraphViz.Attributes
import Data.GraphViz.Commands
import Data.GraphViz.Printing(PrintDot)

import Data.Graph.Inductive.Graph
import qualified Data.Set as Set
import Control.Arrow((&&&))
import Data.Maybe(mapMaybe, isNothing)
import qualified Data.Map as Map
import Control.Monad(liftM)
import System.IO.Unsafe(unsafePerformIO)
import Control.Concurrent(forkIO)

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

{- $conversion
   There are various functions available for converting 'Graph's to
   Graphviz's /Dot/ format (represented using the 'DotGraph' type).
   There are two main types: converting plain graphs and converting
   /clustered/ graphs (where the graph cluster that a particular 'Node'
   belongs to is determined by its label).

   These functions have two versions: one in which the user specifies
   whether the graph is directed or undirected (with a 'Bool' value of
   'True' indicating that the graph is directed), and a primed version
   which attempts to automatically infer if the graph is directed or not.
   Note that these conversion functions assume that undirected graphs
   have every edge being duplicated (or at least that if there exists an
   edge from /n1/ to /n2/, then /n1 <= n2/; if /n1 > n2/ then it is
   removed for an undirected graph).

   Note that the reason these functions do not have 'unsafePerformIO'
   applied to them is because if you set a global 'Attribute' of:
   @
    'Start' ('StartStyle' 'RandomStyle')
   @
   Then it will not necessarily be referentially transparent (ideally,
   no matter what the seed is, it will still eventually be drawn to the
   same optimum, but this can't be guaranteed).  As such, if you are sure
   that you're not using such an 'Attribute', then you should be able to
   use 'unsafePerformIO' directly in your own code.
-}

-- | Convert a graph to Graphviz's /Dot/ format.
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

-- | A pseudo-inverse to 'graphToDot' and 'graphToDot''; \"pseudo\" in
--   the sense that the original node and edge labels aren't able to
--   be reconstructed.
dotToGraph    :: (Graph gr) => DotGraph Node -> gr Attributes Attributes
dotToGraph dg = mkGraph ns' es
  where
    -- Applying uniqBy just in case...
    ns = uniqBy fst . map toLN $ graphNodes dg
    es = concatMap toLE $ graphEdges dg
    -- Need to ensure that for some reason there are node IDs in an
    -- edge but not on their own.
    nSet = Set.fromList $ map fst ns
    nEs = map (flip (,) [])
          . uniq
          . filter (flip Set.notMember nSet)
          $ concatMap (\(n1,n2,_) -> [n1,n2]) es
    ns' = ns ++ nEs
    -- Conversion functions
    toLN (DotNode n as) = (n,as)
    toLE (DotEdge f t d as) = (if d then id else (:) (t,f,as)) [(f,t,as)]


-- | Convert a graph to /Dot/ format, using the specified clustering function
--   to group nodes into clusters.
--   Clusters can be nested to arbitrary depth.
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

{- $augment
   The following functions provide support for passing a 'Graph'
   through the appropriate 'GraphvizCommand' to augment the 'Graph' by
   adding positional information, etc.

   Please note that there are some restrictions on this: to enable
   support for multiple edges between two nodes, the 'Comment'
   'Attribute' is used to provide a unique identifier for each edge.  As
   such, you should /not/ set this 'Attribute' for any 'LEdge'.

   For unprimed functions, the 'Bool' argument is 'True' for directed
   graphs, 'False' otherwise; for the primed versions of functions the
   directionality of the graph is automatically inferred.  Directed
   graphs are passed through 'Dot', and undirected graphs through
   'Neato'.
-}

type AttributeNode a = (Attributes, a)
type AttributeEdge b = (Attributes, b)

-- | Run the appropriate Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.
graphToGraph :: (Graph gr) => Bool -> gr a b -> [GlobalAttributes]
                -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph isDir gr gAttributes fmtNode fmtEdge
    = dotAttributes isDir gr' dot
    where
      dot = graphToDot isDir gr' gAttributes fmtNode (setEdgeComment fmtEdge)
      gr' = addEdgeIDs gr

-- | Run the appropriate Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.
--   Graph direction is automatically inferred.
graphToGraph'    :: (Ord b, Graph gr) => gr a b -> [GlobalAttributes]
                    -> (LNode a -> Attributes) -> (LEdge b -> Attributes)
                    -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph' gr = graphToGraph (isDirected gr) gr

-- | Run the appropriate Graphviz command on the clustered graph to
--   get positional information and then combine that information back
--   into the original graph.
clusterGraphToGraph :: (Ord c, Graph gr) => Bool -> gr a b
                       -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                       -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                       -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                       -> IO (gr (AttributeNode a) (AttributeEdge b))
clusterGraphToGraph isDir gr gAtts clBy cID fmtClust fmtNode fmtEdge
    = dotAttributes isDir gr' dot
    where
      dot = clusterGraphToDot isDir gr' gAtts clBy cID fmtClust fmtNode fmtEdge'
      gr' = addEdgeIDs gr
      fmtEdge' = setEdgeComment fmtEdge

-- | Run the appropriate Graphviz command on the clustered graph to
--   get positional information and then combine that information back
--   into the original graph.
--   Graph direction is automatically inferred.
clusterGraphToGraph'    :: (Ord b, Ord c, Graph gr) => gr a b
                           -> [GlobalAttributes] -> (LNode a -> NodeCluster c l)
                           -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                           -> (LNode l -> Attributes) -> (LEdge b -> Attributes)
                           -> IO (gr (AttributeNode a) (AttributeEdge b))
clusterGraphToGraph' gr = clusterGraphToGraph (isDirected gr) gr

-- -----------------------------------------------------------------------------

{- $quickAugment
   This section contains convenience functions for quick-and-dirty
   augmentation of graphs.  No 'Attribute's are applied, and
   'unsafePerformIO' is used to make these normal functions.  Note that
   this should be safe since these should be referentially transparent
   (as should all augmentation functions).
-}

-- | Pass the graph through 'graphToGraph' with no 'Attribute's.
dotizeGraph         :: (Graph gr) => Bool -> gr a b
                       -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph isDir g = unsafePerformIO
                      $ graphToGraph isDir g gAttrs noAttrs noAttrs
    where
      gAttrs = []
      noAttrs = const []

-- | Pass the graph through 'graphToGraph' with no 'Attribute's.
--   The graph direction is automatically inferred.
dotizeGraph'   :: (Graph gr, Ord b) => gr a b
                  -> gr (AttributeNode a) (AttributeEdge b)
dotizeGraph' g = dotizeGraph (isDirected g) g

-- | Pass the clustered graph through 'clusterGraphToGraph' with no
--   'Attribute's.
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

-- | Pass the clustered graph through 'clusterGraphToGraph' with no
--   'Attribute's.
--   The graph direction is automatically inferred.
dotizeClusterGraph'   :: (Ord b, Ord c, Graph gr) => gr a b
                         -> (LNode a -> NodeCluster c l)
                         -> gr (AttributeNode a) (AttributeEdge b)
dotizeClusterGraph' g = dotizeClusterGraph (isDirected g) g

-- -----------------------------------------------------------------------------

{- $manualAugment
   This section allows you to manually augment graphs by providing
   fine-grained control over the augmentation process (the standard
   augmentation functions compose these together).  For example, you may
   want to be able to access the intermediary 'DotGraph' used to augment
   a 'Graph'.

   Note that whilst these functions provide you with more control, you
   must be careful how you use them: if you use the wrong 'DotGraph' for
   a 'Graph', then the behaviour of 'augmentGraph' (and all functions
   that use them) is undefined.
-}

-- | Used to augment an edge label with a unique identifier.
data EdgeID b = EID { eID  :: String
                    , eLbl :: b
                    }
              deriving (Eq, Ord, Show)
-- Show is only provided for printing/debugging purposes when using a
-- normal Tree-based graph.  Since it doesn't support Read, neither
-- does EdgeID.

-- | Add unique edge identifiers to each label.  This is useful for
--   when multiple edges between two nodes need to be distinguished.
addEdgeIDs   :: (Graph gr) => gr a b -> gr a (EdgeID b)
addEdgeIDs g = mkGraph ns es'
  where
    ns = labNodes g
    es = labEdges g
    es' = zipWith addID es ([1..] :: [Int])
    addID (f,t,l) i = (f,t,EID (show i) l)

-- | Add the 'Comment' to the list of attributes containing the value
--   of the unique edge identifier.
setEdgeComment     :: (LEdge b -> Attributes)
                      -> (LEdge (EdgeID b) -> Attributes)
setEdgeComment f = \ e@(_,_,eid) -> Comment (eID eid) : (f . stripID) e

-- | Remove the unique identifier from the 'LEdge'.
stripID           :: LEdge (EdgeID b) -> LEdge b
stripID (f,t,eid) = (f,t, eLbl eid)

-- | Pass the 'DotGraph' through the relevant command and then augment
--   the 'Graph' that it came from.
dotAttributes :: (Graph gr) => Bool -> gr a (EdgeID b) -> DotGraph Node
                 -> IO (gr (AttributeNode a) (AttributeEdge b))
dotAttributes isDir gr dot
  = liftM (augmentGraph gr . parseDotGraph . fromDotResult)
    $ graphvizWithHandle command dot DotOutput hGetContents'
    where
      command = if isDir then dirCommand else undirCommand

-- | Use the 'Attributes' in the provided 'DotGraph' to augment the
--   node and edge labels in the provided 'Graph'.  The unique
--   identifiers on the edges are also stripped off.
--
--   Please note that the behaviour for this function is undefined if
--   the 'DotGraph' does not come from the original 'Graph' (either
--   by using a conversion function or by passing the result of a
--   conversion function through a 'GraphvizCommand' via the
--   'DotOutput' or similar).
augmentGraph      :: (Graph gr) => gr a (EdgeID b) -> DotGraph Node
                     -> gr (AttributeNode a) (AttributeEdge b)
augmentGraph g dg = mkGraph lns les
  where
    lns = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
          $ labNodes g
    les = map augmentEdge $ labEdges g
    augmentEdge (f,t,(EID eid l)) = (f,t, (edgeMap Map.! eid, l))
    ns = graphNodes dg
    es = graphEdges dg
    nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
    edgeMap = Map.fromList $ map (findID &&& edgeAttributes') es
    findID = head . mapMaybe commentID . edgeAttributes
    commentID (Comment s) = Just s
    commentID _           = Nothing
    -- Strip out the comment
    edgeAttributes' = filter (isNothing . commentID) . edgeAttributes


-- -----------------------------------------------------------------------------
-- Utility Functions

-- | Pretty-print the 'DotGraph' by passing it through the 'Canon'
--   output type (which produces \"canonical\" output).  This is
--   required because the @printIt@ function in
--   "Data.GraphViz.Types.Printing" no longer uses indentation to
--   ensure the Dot code is printed correctly.
prettyPrint    :: (PrintDot a) => DotGraph a -> IO String
prettyPrint dg = liftM fromDotResult
                 -- Note that the choice of command here should be
                 -- arbitrary.
                 $ graphvizWithHandle (commandFor dg)
                                      dg
                                      Canon
                                      hGetContents'

-- | The 'unsafePerformIO'd version of 'prettyPrint'.  Graphviz should
--   always produce the same pretty-printed output, so this should be
--   safe.
prettyPrint' :: (PrintDot a) => DotGraph a -> String
prettyPrint' = unsafePerformIO . prettyPrint

-- | Quickly visualise a graph using the 'Xlib' 'GraphvizCanvas'.
preview   :: (Ord b, Graph gr) => gr a b -> IO ()
preview g = ign $ forkIO (ign $ runGraphvizCanvas' dg Xlib)
  where
    dg = graphToDot' g [] (const []) (const [])
    ign = (>> return ())

-- | Used for obtaining results from 'graphvizWithHandle', etc. when
--   errors should only occur when Graphviz isn't installed.  If the
--   value is @'Left' _@, then 'error' is used.
fromDotResult           :: Either l r -> r
fromDotResult (Right r) = r
fromDotResult Left{}    = error "Could not run the relevant Graphviz command; \
                                 \is the Graphviz suite of tools installed?"
