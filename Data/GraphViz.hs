{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}

{- |
   Module      : Data.GraphViz
   Description : Graphviz bindings for Haskell.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is the top-level module for the graphviz library.  It provides
   functions to convert 'Data.Graph.Inductive.Graph.Graph's into the
   /Dot/ language used by the /Graphviz/ suite of programs (as well as a
   limited ability to perform the reverse operation).

   If you wish to construct a Haskell representation of a Dot graph
   yourself rather than using the conversion functions here, please
   see the "Data.GraphViz.Types" module as a starting point for how to
   do so.

   Information about Graphviz and the Dot language can be found at:
   <http://graphviz.org/>
 -}

module Data.GraphViz
    ( -- * Conversion from graphs to /Dot/ format.
      -- ** Specifying parameters.
      -- $params
      GraphvizParams(..)
    , quickParams
    , defaultParams
    , nonClusteredParams
    , blankParams
    , setDirectedness
      -- *** Specifying clusters.
    , NodeCluster(..)
    , LNodeCluster
      -- ** Converting graphs.
    , graphToDot
    , graphElemsToDot
      -- ** Pseudo-inverse conversion.
    , dotToGraph
      -- * Graph augmentation.
      -- $augment
      -- ** Type aliases for @Node@ and @Edge@ labels.
    , AttributeNode
    , AttributeEdge
      -- ** Customisable augmentation.
    , graphToGraph
      -- ** Quick augmentation.
    , dotizeGraph
      -- ** Manual augmentation.
      -- $manualAugment
    , EdgeID
    , addEdgeIDs
    , setEdgeIDAttribute
    , dotAttributes
    , augmentGraph
      -- * Utility functions
    , preview
      -- * Re-exporting other modules.
    , module Data.GraphViz.Types
    , module Data.GraphViz.Types.Canonical
    , module Data.GraphViz.Attributes
    , module Data.GraphViz.Commands
    ) where

import Data.GraphViz.Algorithms.Clustering
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete   (AttributeName, CustomAttribute,
                                            customAttribute, customValue,
                                            findSpecifiedCustom)
import Data.GraphViz.Commands
import Data.GraphViz.Commands.IO           (hGetDot)
import Data.GraphViz.Internal.Util         (uniq, uniqBy)
import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical       (DotGraph (..), DotStatements (..),
                                            DotSubGraph (..))
import Data.GraphViz.Types.Generalised     (FromGeneralisedDot (..))

import           Control.Arrow              (first, (&&&))
import           Control.Concurrent         (forkIO)
import           Data.Graph.Inductive.Graph
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, mapMaybe)
import qualified Data.Set                   as Set
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as T
import           System.IO.Unsafe           (unsafePerformIO)

#if !(MIN_VERSION_base (4,8,0))
import Data.Functor ((<$>))
#endif

-- -----------------------------------------------------------------------------

-- | Determine if the given graph is undirected.
isUndirected   :: (Ord b, Graph g) => g a b -> Bool
isUndirected g = all hasFlip es
  where
    es = labEdges g
    eSet = Set.fromList es
    hasFlip e = Set.member (flippedEdge e) eSet
    flippedEdge (f,t,l) = (t,f,l)

-- -----------------------------------------------------------------------------

{- $params

   A 'GraphvizParams' value contains all the information necessary to
   manipulate 'Graph's with this library.  As such, its components deal
   with:

   * Whether to treat graphs as being directed or not;

   * Which top-level 'GlobalAttributes' values should be applied;

   * How to define (and name) clusters;

   * How to format clusters, nodes and edges.

   Apart from not having to pass multiple values around, another
   advantage of using 'GraphvizParams' over the previous approach is that
   there is no distinction between clustering and non-clustering variants
   of the same functions.

   Example usages of 'GraphvizParams' follow:

   * Quickly visualise a graph using the default parameters.  Note the
     usage of @'nonClusteredParams'@ over @'defaultParams'@ to avoid
     type-checking problems with the cluster type.

     > defaultVis :: (Graph gr) => gr nl el -> DotGraph Node
     > defaultVis = graphToDot nonClusteredParams

   * As with @defaultVis@, but determine whether or not the graph is
     directed or undirected.

     > checkDirectednessVis :: (Graph gr, Ord el) => gr nl el -> DotGraph Node
     > checkDirectednessVis = setDirectedness graphToDot nonClusteredParams

   * Clustering nodes based upon whether they are even or odd.  We
     have the option of either constructing a @GraphvizParams@
     directly, or using @'blankParams'@.  Using the latter to avoid
     setting @'isDirected'@:

     > evenOdd :: (Graph gr, Ord el) => gr Int el -> DotGraph Node
     > evenOdd = setDirectedness graphToDot params
     >   where
     >     params = blankParams { globalAttributes = []
     >                          , clusterBy        = clustBy
     >                          , clusterID        = Num . Int
     >                          , fmtCluster       = clFmt
     >                          , fmtNode          = const []
     >                          , fmtEdge          = const []
     >                          }
     >     clustBy (n,l) = C (n `mod` 2) $ N (n,l)
     >     clFmt m = [GraphAttrs [toLabel $ "n == " ++ show m ++ " (mod 2)"]]

   For more examples, see the source of 'dotizeGraph' and 'preview'.

 -}

-- | Defines the parameters used to convert a 'Graph' into a 'DotRepr'.
--
--   A value of type @'GraphvizParams' n nl el cl l@ indicates that
--   the 'Graph' has a node type of @n@, node labels of type @nl@,
--   edge labels of type @el@, corresponding clusters of type @cl@ and
--   after clustering the nodes have a label of type @l@ (which may or
--   may not be the same as @nl@).
--
--   The tuples in the function types represent labelled nodes (for
--   @(n,nl)@ and @(n,l)@) and labelled edges (@(n,n,el)@; the value
--   @(f,t,ftl)@ is an edge from @f@ to @l@ with a label of @ftl@).
--   These correspond to 'LNode' and 'LEdge' in FGL graphs.
--
--   The clustering in 'clusterBy' can be to arbitrary depth.
--
--   Note that the term \"cluster\" is slightly conflated here: in
--   terms of @GraphvizParams@ values, a cluster is a grouping of
--   nodes; the 'isDotCluster' function lets you specify whether it is
--   a cluster in the Dot sense or just a sub-graph.
data GraphvizParams n nl el cl l
     = Params { -- | @True@ if the graph is directed; @False@
                --   otherwise.
                isDirected       :: Bool
                -- | The top-level global 'Attributes' for the entire
                --   graph.
              , globalAttributes :: [GlobalAttributes]
                -- | A function to specify which cluster a particular
                --   node is in.
              , clusterBy        :: ((n,nl) -> NodeCluster cl (n,l))
                -- | Is this \"cluster\" actually a cluster, or just a
                --   sub-graph?
              , isDotCluster     :: (cl -> Bool)
                -- | The name/identifier for a cluster.
              , clusterID        :: (cl -> GraphID)
                -- | Specify which global attributes are applied in
                --   the given cluster.
              , fmtCluster       :: (cl -> [GlobalAttributes])
                -- | The specific @Attributes@ for a node.
              , fmtNode          :: ((n,l) -> Attributes)
                -- | The specific @Attributes@ for an edge.
              , fmtEdge          :: ((n,n,el) -> Attributes)
              }


-- | An alias for 'NodeCluster' when dealing with FGL graphs.
type LNodeCluster cl l = NodeCluster cl (Node,l)

-- | Especially useful for quick explorations in ghci, this is a "do
--   what I mean" set of parameters that prints the specified labels
--   of a non-clustered graph.
quickParams :: (Labellable nl, Labellable el) => GraphvizParams n nl el () nl
quickParams = nonClusteredParams { fmtNode = nodeFmt, fmtEdge = edgeFmt }
  where
    nodeFmt (_,l) = [toLabel l]
    edgeFmt (_,_,l) = [toLabel l]

-- | A default 'GraphvizParams' value which assumes the graph is
--   directed, contains no clusters and has no 'Attribute's set.
--
--   If you wish to have the labels of the nodes to have a different
--   type after applying 'clusterBy' from before clustering, then you
--   will have to specify your own 'GraphvizParams' value from
--   scratch (or use 'blankParams').
--
--   If you use a custom 'clusterBy' function (which if you actually
--   want clusters you should) then you should also override the
--   (nonsensical) default 'clusterID'.
defaultParams :: GraphvizParams n nl el cl nl
defaultParams = Params { isDirected       = True
                       , globalAttributes = []
                       , clusterBy        = N
                       , isDotCluster     = const True
                       , clusterID        = const (Num $ Int 0)
                       , fmtCluster       = const []
                       , fmtNode          = const []
                       , fmtEdge          = const []
                       }

-- | A variant of 'defaultParams' that enforces that the clustering
--   type is @'()'@ (i.e.: no clustering); this avoids problems when
--   using 'defaultParams' internally within a function without any
--   constraint on what the clustering type is.
nonClusteredParams :: GraphvizParams n nl el () nl
nonClusteredParams = defaultParams

-- | A 'GraphvizParams' value where every field is set to
--   @'undefined'@.  This is useful when you have a function that will
--   set some of the values for you (e.g. 'setDirectedness') but you
--   don't want to bother thinking of default values to set in the
--   meantime.  This is especially useful when you are
--   programmatically setting the clustering function (and as such do
--   not know what the types might be).
blankParams :: GraphvizParams n nl el cl l
blankParams = Params { isDirected       = error "Unspecified definition of isDirected"
                     , globalAttributes = error "Unspecified definition of globalAttributes"
                     , clusterBy        = error "Unspecified definition of clusterBy"
                     , isDotCluster     = error "Unspecified definition of isDotCluster"
                     , clusterID        = error "Unspecified definition of clusterID"
                     , fmtCluster       = error "Unspecified definition of fmtCluster"
                     , fmtNode          = error "Unspecified definition of fmtNode"
                     , fmtEdge          = error "Unspecified definition of fmtEdge"
                     }

-- | Determine if the provided 'Graph' is directed or not and set the
--   value of 'isDirected' appropriately.
setDirectedness             :: (Ord el, Graph gr)
                               => (GraphvizParams Node nl el cl l -> gr nl el -> a)
                               -> GraphvizParams Node nl el cl l -> gr nl el -> a
setDirectedness f params gr = f params' gr
  where
    params' = params { isDirected = not $ isUndirected gr }

-- | Convert a graph to /Dot/ format, using the specified parameters
--   to cluster the graph, etc.
graphToDot :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l
              -> gr nl el -> DotGraph Node
graphToDot params graph = graphElemsToDot params (labNodes graph) (labEdges graph)

-- | As with 'graphToDot', but this allows you to easily convert other
--   graph-like formats to a Dot graph as long as you can get a list
--   of nodes and edges from it.
graphElemsToDot :: (Ord cl, Ord n) => GraphvizParams n nl el cl l
                   -> [(n,nl)] -> [(n,n,el)] -> DotGraph n
graphElemsToDot params lns les
  = DotGraph { strictGraph     = False
             , directedGraph   = dirGraph
             , graphID         = Nothing
             , graphStatements = stmts
             }
  where
    dirGraph = isDirected params
    stmts = DotStmts { attrStmts = globalAttributes params
                     , subGraphs = cs
                     , nodeStmts = ns
                     , edgeStmts = es
                     }
    (cs, ns) = clustersToNodes (clusterBy params) (isDotCluster params)
                               (clusterID params) (fmtCluster params) (fmtNode params)
                               lns
    es = mapMaybe mkDotEdge les
    mkDotEdge e@(f,t,_) = if dirGraph || f <= t
                          then Just
                               DotEdge { fromNode       = f
                                       , toNode         = t
                                       , edgeAttributes = fmtEdge params e
                                       }
                          else Nothing

-- | A pseudo-inverse to 'graphToDot'; \"pseudo\" in the sense that
--   the original node and edge labels aren't able to be
--   reconstructed.
dotToGraph    :: (DotRepr dg Node, Graph gr) => dg Node
                 -> gr Attributes Attributes
dotToGraph dg = mkGraph ns' es
  where
    d = graphIsDirected dg
    -- Applying uniqBy just in case...
    ns = uniqBy fst . map toLN $ graphNodes dg
    es = concatMap toLE $ graphEdges dg
    -- Need to check that for some reason there aren't node IDs in an
    -- edge but not on their own.
    nSet = Set.fromList $ map fst ns
    nEs = map (flip (,) [])
          . uniq
          . filter (`Set.notMember` nSet)
          $ concatMap (\(n1,n2,_) -> [n1,n2]) es
    ns' = ns ++ nEs
    -- Conversion functions
    toLN (DotNode n as) = (n,as)
    toLE (DotEdge f t as) = (if d then id else (:) (t,f,as)) [(f,t,as)]

-- -----------------------------------------------------------------------------

{- $augment
   The following functions provide support for passing a 'Graph'
   through the appropriate 'GraphvizCommand' to augment the 'Graph' by
   adding positional information, etc.

   A 'CustomAttribute' is used to distinguish multiple edges between
   two nodes from each other.

   Note that the reason that most of these functions do not have
   'unsafePerformIO' applied to them is because if you set a global
   'Attribute' of:

   @
    'Start' ('StartStyle' 'RandomStyle')
   @

   then it will not necessarily be referentially transparent (ideally,
   no matter what the seed is, it will still eventually be drawn to the
   same optimum, but this can't be guaranteed).  As such, if you are sure
   that you're not using such an 'Attribute', then you should be able to
   use 'unsafePerformIO' directly in your own code.
-}

-- | Augment the current node label type with the 'Attributes' applied
--   to that node.
type AttributeNode nl = (Attributes, nl)

-- | Augment the current edge label type with the 'Attributes' applied
--   to that edge.
type AttributeEdge el = (Attributes, el)

-- | Run the appropriate Graphviz command on the graph to get
--   positional information and then combine that information back
--   into the original graph.
graphToGraph :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l -> gr nl el
                -> IO (gr (AttributeNode nl) (AttributeEdge el))
graphToGraph params gr = dotAttributes (isDirected params) gr' dot
  where
    dot = graphToDot params' gr'
    params' = params { fmtEdge = setEdgeIDAttribute $ fmtEdge params }
    gr' = addEdgeIDs gr

-- -----------------------------------------------------------------------------

-- | This is a \"quick-and-dirty\" graph augmentation function that
--   sets no 'Attributes' and thus should be referentially transparent
--   and is wrapped in 'unsafePerformIO'.
--
--   Note that the provided 'GraphvizParams' is only used for
--   'isDirected', 'clusterBy' and 'clusterID'.
dotizeGraph           :: (Ord cl, Graph gr) => GraphvizParams Node nl el cl l
                         -> gr nl el -> gr (AttributeNode nl) (AttributeEdge el)
dotizeGraph params gr = unsafePerformIO
                        $ graphToGraph params' gr
  where
    params' = params { fmtCluster = const []
                     , fmtNode    = const []
                     , fmtEdge    = const []
                     }

-- -----------------------------------------------------------------------------

{- $manualAugment

   This section allows you to manually augment graphs by providing
   fine-grained control over the augmentation process (the standard
   augmentation functions compose these together).  Possible reasons for
   manual augmentation are:

   * Gain access to the intermediary 'DotRepr' used.

   * Convert the default 'DotGraph' to a @GDotGraph@ (found in
     "Data.GraphViz.Types.Generalised") so as to have greater control over
     the generated Dot code.

   * Use a specific 'GraphvizCommand' rather than the default.

   Note that whilst these functions provide you with more control, you
   must be careful how you use them: if you use the wrong 'DotRepr' for
   a 'Graph', then the behaviour of 'augmentGraph' (and all functions
   that use it) is undefined.  The main point is to make sure that the
   defined 'DotNode' and 'DotEdge' values aren't removed (or their ID
   values - or the 'Attributes' for the 'DotEdge's - altered) to
   ensure that it is possible to match up the nodes and edges in the
   'Graph' with those in the 'DotRepr'.

-}

-- | Used to augment an edge label with a unique identifier.
data EdgeID el = EID { eID  :: Text
                     , eLbl :: el
                     }
               deriving (Eq, Ord, Show)
-- Show is only provided for printing/debugging purposes when using a
-- normal Tree-based graph.  Since it doesn't support Read, neither
-- does EdgeID.

-- | Add unique edge identifiers to each label.  This is useful for
--   when multiple edges between two nodes need to be distinguished.
addEdgeIDs   :: (Graph gr) => gr nl el -> gr nl (EdgeID el)
addEdgeIDs g = mkGraph ns es'
  where
    ns = labNodes g
    es = labEdges g
    es' = zipWith addID es ([1..] :: [Int])
    addID (f,t,l) i = (f,t,EID (T.pack $ show i) l)

-- | Add a custom attribute to the list of attributes containing the
--   value of the unique edge identifier.
setEdgeIDAttribute     :: (LEdge el -> Attributes)
                          -> (LEdge (EdgeID el) -> Attributes)
setEdgeIDAttribute f = \ e@(_,_,eid) -> identifierAttribute (eID eid)
                                        : (f . stripID) e

identifierAttrName :: AttributeName
identifierAttrName = "graphviz_distinguish_multiple_edges"

identifierAttribute :: Text -> CustomAttribute
identifierAttribute = customAttribute identifierAttrName

-- | Remove the unique identifier from the 'LEdge'.
stripID           :: LEdge (EdgeID el) -> LEdge el
stripID (f,t,eid) = (f,t, eLbl eid)

-- | Pass the 'DotRepr' through the relevant command and then augment
--   the 'Graph' that it came from.
dotAttributes :: (Graph gr, PPDotRepr dg Node, FromGeneralisedDot dg Node)
                 => Bool -> gr nl (EdgeID el)
                 -> dg Node -> IO (gr (AttributeNode nl) (AttributeEdge el))
dotAttributes isDir gr dot
  = augmentGraph gr . parseDG <$> graphvizWithHandle command dot DotOutput hGetDot
  where
    parseDG = (`asTypeOf` dot) . fromGeneralised
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
augmentGraph      :: (Graph gr, DotRepr dg Node) => gr nl (EdgeID el)
                     -> dg Node -> gr (AttributeNode nl) (AttributeEdge el)
augmentGraph g dg = mkGraph lns les
  where
    lns = map (\(n, l) -> (n, (nodeMap Map.! n, l)))
          $ labNodes g
    les = map augmentEdge $ labEdges g
    augmentEdge (f,t,EID eid l) = (f,t, (edgeMap Map.! eid, l))
    ns = graphNodes dg
    es = graphEdges dg
    nodeMap = Map.fromList $ map (nodeID &&& nodeAttributes) ns
    edgeMap = Map.fromList $ map edgeIDAttrs es
    edgeIDAttrs = first customValue . fromJust
                  . findSpecifiedCustom identifierAttrName
                  . edgeAttributes

-- -----------------------------------------------------------------------------
-- Utility Functions

-- | Quickly visualise a graph using the 'Xlib' 'GraphvizCanvas'.  If
--   your label types are not (and cannot) be instances of 'Labellable',
--   you may wish to use 'gmap', 'nmap' or 'emap' to set them to a value
--   such as @\"\"@.
preview   :: (Ord el, Graph gr, Labellable nl, Labellable el) => gr nl el -> IO ()
preview g = ign $ forkIO (ign $ runGraphvizCanvas' dg Xlib)
  where
    dg = setDirectedness graphToDot params g
    params = nonClusteredParams { fmtNode = \ (_,l) -> [toLabel l]
                                , fmtEdge = \ (_, _, l) -> [toLabel l]
                                }
    ign = (>> return ())
