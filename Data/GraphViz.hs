{-# LANGUAGE   MultiParamTypeClasses, FlexibleContexts, OverloadedStrings #-}

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
      -- ** Specifying parameters.
      -- $params
      GraphvizParams(..)
    , defaultParams
    , nonClusteredParams
    , blankParams
    , setDirectedness
      -- *** Specifying clusters.
    , LNodeCluster
    , NodeCluster(..)
      -- ** Converting graphs.
    , graphToDot
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

import Data.GraphViz.Types
import Data.GraphViz.Types.Canonical( DotGraph(..), DotStatements(..)
                                    , DotSubGraph(..))
import Data.GraphViz.Algorithms.Clustering
import Data.GraphViz.Util(uniq, uniqBy)
import Data.GraphViz.Attributes
import Data.GraphViz.Attributes.Complete(CustomAttribute, AttributeName
                                        , customAttribute, findSpecifiedCustom
                                        , customValue)
import Data.GraphViz.Commands
import Data.GraphViz.Commands.IO(hGetDot)

import Data.Graph.Inductive.Graph
import qualified Data.Set as Set
import Control.Arrow((&&&), first)
import Data.Maybe(mapMaybe, fromJust)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Data.Text.Lazy(Text)
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

   * Clustering nodes based upon whether they are even or odd.  We have
     the option of either constructing a @GraphvizParams@ directly, or
     using @'blankParams'@.  Going with the latter to avoid setting
     @'isDirected'@.

     > evenOdd :: (Graph gr, Ord el) => gr Int el -> DotGraph Node
     > evenOdd = setDirectedness graphToDot params
     >   where
     >     params = blankParams { globalAttributes = []
     >                          , clusterBy        = clustBy
     >                          , clusterID        = Just . Int
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
--   A value of type @'GraphvizParams' nl el cl l@ indicates that the
--   'Graph' has node labels of type @nl@, edge labels of type @el@,
--   corresponding clusters of type @cl@ and after clustering the
--   nodes have a label of type @l@ (which may or may not be the same
--   as @nl@).
--
--   The clustering in 'clusterBy' can be to arbitrary depth.
data GraphvizParams nl el cl l
     = Params { -- | @True@ if the graph is directed; @False@
                --   otherwise.
                isDirected       :: Bool
                -- | The top-level global 'Attributes' for the entire
                --   graph.
              , globalAttributes :: [GlobalAttributes]
                -- | A function to specify which cluster a particular
                --   'LNode' is in.
              , clusterBy        :: (LNode nl -> LNodeCluster cl l)
                -- | The name/identifier for a cluster.
              , clusterID        :: (cl -> Maybe GraphID)
                -- | Specify which global attributes are applied in
                --   the given cluster.
              , fmtCluster       :: (cl -> [GlobalAttributes])
                -- | The specific @Attributes@ for a node.
              , fmtNode          :: (LNode l -> Attributes)
                -- | The specific @Attributes@ for an edge.
              , fmtEdge          :: (LEdge el -> Attributes)
              }

-- | A default 'GraphvizParams' value which assumes the graph is
--   directed, contains no clusters and has no 'Attribute's set.
--
--   If you wish to have the labels of the nodes to have a different
--   type after applying 'clusterBy' from before clustering, then you
--   will have to specify your own 'GraphvizParams' value from
--   scratch (or use 'blankParams').
defaultParams :: GraphvizParams nl el cl nl
defaultParams = Params { isDirected       = True
                       , globalAttributes = []
                       , clusterBy        = N
                       , clusterID        = const Nothing
                       , fmtCluster       = const []
                       , fmtNode          = const []
                       , fmtEdge          = const []
                       }

-- | A variant of 'defaultParams' that enforces that the clustering
--   type is @'()'@ (i.e.: no clustering); this avoids problems when
--   using 'defaultParams' internally within a function without any
--   constraint on what the clustering type is.
nonClusteredParams :: GraphvizParams nl el () nl
nonClusteredParams = defaultParams

-- | A 'GraphvizParams' value where every field is set to
--   @'undefined'@.  This is useful when you have a function that will
--   set some of the values for you (e.g. 'setDirectedness') but you
--   don't want to bother thinking of default values to set in the
--   meantime.  This is especially useful when you are
--   programmatically setting the clustering function (and as such do
--   not know what the types might be).
blankParams :: GraphvizParams nl el cl l
blankParams = Params { isDirected       = undefined
                     , globalAttributes = undefined
                     , clusterBy        = undefined
                     , clusterID        = undefined
                     , fmtCluster       = undefined
                     , fmtNode          = undefined
                     , fmtEdge          = undefined
                     }

-- | Determine if the provided 'Graph' is directed or not and set the
--   value of 'isDirected' appropriately.
setDirectedness             :: (Ord el, Graph gr)
                               => (GraphvizParams nl el cl l -> gr nl el -> a)
                               -> GraphvizParams nl el cl l -> gr nl el -> a
setDirectedness f params gr = f params' gr
  where
    params' = params { isDirected = not $ isUndirected gr }

-- | Convert a graph to /Dot/ format, using the specified parameters
--   to cluster the graph, etc.
graphToDot :: (Ord cl, Graph gr) => GraphvizParams nl el cl l
              -> gr nl el -> DotGraph Node
graphToDot params graph
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
    (cs, ns) = clustersToNodes (clusterBy params) (clusterID params)
                               (fmtCluster params) (fmtNode params)
                               graph
    es = mapMaybe mkDotEdge . labEdges $ graph
    mkDotEdge e@(f,t,_) = if dirGraph || f <= t
                          then Just
                               DotEdge { edgeFromNodeID = f
                                       , edgeToNodeID   = t
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
    -- Need to ensure that for some reason there are node IDs in an
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
graphToGraph :: (Ord cl, Graph gr) => GraphvizParams nl el cl l -> gr nl el
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
dotizeGraph           :: (Ord cl, Graph gr) => GraphvizParams nl el cl l
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

-- | Add the 'Comment' to the list of attributes containing the value
--   of the unique edge identifier.
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
dotAttributes :: (Graph gr, DotRepr dg Node) => Bool -> gr nl (EdgeID el)
                 -> dg Node -> IO (gr (AttributeNode nl) (AttributeEdge el))
dotAttributes isDir gr dot
  = liftM (augmentGraph gr . parseDG)
    $ graphvizWithHandle command dot DotOutput hGetDot
  where
    parseDG = asTypeOf dot
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
