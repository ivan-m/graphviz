{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
   Module      : Data.GraphViz.Types
   Description : Haskell representation of Dot graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Various different representations of Dot graphs are available, all
   of which are based loosely upon the specifications at:
     <http://graphviz.org/doc/info/lang.html>
   The 'DotRepr' class provides a common interface for them.

   Printing of /Dot/ code is done as strictly as possible, whilst
   parsing is as permissive as possible.  For example, if the types
   allow it then @\"2\"@ will be parsed as an 'Int' value.  Note that
   quoting and escaping of 'String' values is done automagically.

   A summary of known limitations\/differences:

   * When creating 'GraphID' values for graphs and sub-graphs,
     you should ensure that none of them have the same printed value
     as one of the node identifiers values to avoid any possible problems.

   * If you want any 'GlobalAttributes' in a sub-graph and want
     them to only apply to that sub-graph, then you must ensure it
     does indeed have a valid 'GraphID'.

   * All sub-graphs which represent clusters should have unique
     identifiers (well, only if you want them to be generated
     sensibly).

   * If eventually outputting to a format such as SVG, then you should
     make sure to specify an identifier for the overall graph, as that is
     used as the title of the resulting image.

   * Whilst the graphs, etc. are polymorphic in their node type, you
     should ensure that you use a relatively simple node type (that
     is, it only covers a single line, etc.).

   * Also, whilst Graphviz allows you to mix the types used for nodes,
     this library requires\/assumes that they are all the same type (but
     you /can/ use a sum-type).

   * 'DotEdge' defines an edge @(a, b)@ (with an edge going from @a@
     to @b@); in /Dot/ parlance the edge has a head at @a@ and a tail
     at @b@.  Care must be taken when using the related @Head*@ and
     @Tail*@ 'Attribute's.  See the differences section in
     "Data.GraphViz.Attributes" for more information.

   * It is common to see multiple edges defined on the one line in Dot
     (e.g. @n1 -> n2 -> n3@ means to create a directed edge from @n1@
     to @n2@ and from @n2@ to @n3@).  These types of edge definitions
     are parseable; however, they are converted to singleton edges.

   * It is not yet possible to create or parse edges with
     subgraphs\/clusters as one of the end points.

   * The parser will strip out comments and pre-processor lines, join
     together multiline statements and concatenate split strings together.
     However, pre-processing within HTML-like labels is currently not
     supported.

   * Graphviz allows a node to be \"defined\" twice (e.g. the actual
     node definition, and then in a subgraph with extra global attributes
     applied to it).  This actually represents the same node, but when
     parsing they will be considered as separate 'DotNode's (such that
     'graphNodes' will return both \"definitions\").  @canonicalise@ from
     "Data.GraphViz.Algorithms" can be used to fix this.

   See "Data.GraphViz.Attributes" for more limitations.

-}
module Data.GraphViz.Types
       ( DotRepr(..)
         -- * Common sub-types
       , GraphID(..)
       , GlobalAttributes(..)
       , DotNode(..)
       , DotEdge(..)
         -- * Helper types for looking up information within a @DotRepr@.
       , ClusterLookup
       , NodeLookup
       , Path
         -- * Obtaining the @DotNode@s and @DotEdges@.
       , graphNodes
       , graphEdges
         -- * Printing and parsing a @DotRepr@.
       , printDotGraph
       , parseDotGraph
       ) where

import Data.GraphViz.Types.Canonical( DotGraph(..), DotStatements(..)
                                    , DotSubGraph(..))
import Data.GraphViz.Types.Common( GraphID(..), GlobalAttributes(..)
                                 , DotNode(..), DotEdge(..))
import Data.GraphViz.Types.State
import Data.GraphViz.Util(bool)
import Data.GraphViz.Parsing(ParseDot, parseIt)
import Data.GraphViz.PreProcessing(preProcess)
import Data.GraphViz.Printing(PrintDot, printIt)

import Data.Text.Lazy(Text)

-- -----------------------------------------------------------------------------

-- | This class is used to provide a common interface to different
--   ways of representing a graph in /Dot/ form.
class (PrintDot (dg n), ParseDot (dg n)) => DotRepr dg n where
  -- | Convert from a graph in canonical form.  This is especially
  --   useful when using the functions from "Data.GraphViz.Algorithms".
  fromCanonical :: DotGraph n -> dg n

  -- | Return the ID of the graph.
  getID :: dg n -> Maybe GraphID

  -- | Set the ID of the graph.
  setID :: GraphID -> dg n -> dg n

  -- | Is this graph directed?
  graphIsDirected :: dg n -> Bool

  -- | Set whether a graph is directed or not.
  setIsDirected :: Bool -> dg n -> dg n

  -- | Is this graph strict?
  graphIsStrict :: dg n -> Bool

  -- | A strict graph disallows multiple edges.
  setStrictness :: Bool -> dg n -> dg n

  -- | Return information on all the clusters contained within this
  --   'DotRepr', as well as the top-level 'GraphAttrs' for the
  --   overall graph.
  graphStructureInformation :: dg n -> (GlobalAttributes, ClusterLookup)

  -- | Return information on the 'DotNode's contained within this
  --   'DotRepr'.  The 'Bool' parameter indicates if applicable
  --   'NodeAttrs' should be included.
  nodeInformation :: Bool -> dg n -> NodeLookup n

  -- | Return information on the 'DotEdge's contained within this
  --   'DotRepr'.  The 'Bool' parameter indicates if applicable
  --   'EdgeAttrs' should be included.
  edgeInformation :: Bool -> dg n -> [DotEdge n]

-- | Returns all resultant 'DotNode's in the 'DotRepr' (not including
--   'NodeAttr's).
graphNodes :: (DotRepr dg n, Ord n) => dg n -> [DotNode n]
graphNodes = toDotNodes . nodeInformation False

-- | Returns all resultant 'DotEdge's in the 'DotRepr' (not including
--   'EdgeAttr's).
graphEdges :: (DotRepr dg n) => dg n -> [DotEdge n]
graphEdges = edgeInformation False

-- | The actual /Dot/ code for an instance of 'DotRepr'.  Note that it
--   is expected that @'parseDotGraph' . 'printDotGraph' == 'id'@
--   (this might not be true the other way around due to un-parseable
--   components).
printDotGraph :: (DotRepr dg n) => dg n -> Text
printDotGraph = printIt

-- | Parse a limited subset of the Dot language to form an instance of
--   'DotRepr'.  Each instance may have its own limitations on what
--   may or may not be parseable Dot code.
--
--   Also removes any comments, etc. before parsing.
parseDotGraph :: (DotRepr dg n) => Text -> dg n
parseDotGraph = fst . parseIt . preProcess

-- -----------------------------------------------------------------------------
-- Instance for Canonical graphs, to avoid cyclic modules.

instance (Ord n, PrintDot n, ParseDot n) => DotRepr DotGraph n where
  fromCanonical = id

  getID = graphID

  setID i g = g { graphID = Just i }

  graphIsDirected = directedGraph

  setIsDirected d g = g { directedGraph = d }

  graphIsStrict = strictGraph

  setStrictness s g = g { strictGraph = s }

  graphStructureInformation = getGraphInfo
                              . statementStructure . graphStatements

  nodeInformation wGlobal = getNodeLookup wGlobal
                            . statementNodes . graphStatements

  edgeInformation wGlobal = getDotEdges wGlobal
                            . statementEdges . graphStatements

statementStructure :: DotStatements n -> GraphState ()
statementStructure stmts
  = do mapM_ addGraphGlobals $ attrStmts stmts
       mapM_ (withSubGraphID addSubGraph statementStructure) $ subGraphs stmts

statementNodes :: (Ord n) => DotStatements n -> NodeState n ()
statementNodes stmts
  = do mapM_ addNodeGlobals $ attrStmts stmts
       mapM_ (withSubGraphID recursiveCall statementNodes) $ subGraphs stmts
       mapM_ addNode $ nodeStmts stmts
       mapM_ addEdgeNodes $ edgeStmts stmts

statementEdges :: DotStatements n -> EdgeState n ()
statementEdges stmts
  = do mapM_ addEdgeGlobals $ attrStmts stmts
       mapM_ (withSubGraphID recursiveCall statementEdges) $ subGraphs stmts
       mapM_ addEdge $ edgeStmts stmts

withSubGraphID        :: (Maybe (Maybe GraphID) -> b -> a)
                         -> (DotStatements n -> b) -> DotSubGraph n -> a
withSubGraphID f g sg = f mid . g $ subGraphStmts sg
  where
    mid = bool Nothing (Just $ subGraphID sg) $ isCluster sg
