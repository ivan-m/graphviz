{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TypeSynonymInstances #-}

{- |
   Module      : Data.GraphViz.Types
   Description : Haskell representation of Dot graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Four different representations of Dot graphs are available, all of
   which are based loosely upon the specifications at:
   <http://graphviz.org/doc/info/lang.html>.  The 'DotRepr' class
   provides a common interface for them (the 'PrintDotRepr',
   'ParseDotRepr' and 'PPDotRepr' classes are used until class aliases
   are implemented).

   Every representation takes in a type parameter: this indicates the
   node type (e.g. @DotGraph Int@ is a Dot graph with integer nodes).
   Sum types are allowed, though care must be taken when specifying
   their 'ParseDot' instances if there is the possibility of
   overlapping definitions.  The 'GraphID' type is an existing sum
   type that allows textual and numeric values.

   If you require using more than one Dot representation, you will
   most likely need to import at least one of them qualified, as they
   typically all use the same names.

   As a comparison, all four representations provide how you would
   define the following Dot graph (or at least one isomorphic to it)
   (the original of which can be found at
   <http://graphviz.org/content/cluster>).  Note that in all the
   examples, they are not necessarily done the best way (variables
   rather than repeated constants, etc.); they are just there to
   provide a comparison on the structure of each representation.

   > digraph G {
   >
   >   subgraph cluster_0 {
   >     style=filled;
   >     color=lightgrey;
   >     node [style=filled,color=white];
   >     a0 -> a1 -> a2 -> a3;
   >     label = "process #1";
   >   }
   >
   >   subgraph cluster_1 {
   >     node [style=filled];
   >     b0 -> b1 -> b2 -> b3;
   >     label = "process #2";
   >     color=blue
   >   }
   >   start -> a0;
   >   start -> b0;
   >   a1 -> b3;
   >   b2 -> a3;
   >   a3 -> a0;
   >   a3 -> end;
   >   b3 -> end;
   >
   >   start [shape=Mdiamond];
   >   end [shape=Msquare];
   > }

    Each representation is suited for different things:

    ["Data.GraphViz.Types.Canonical"] is ideal for converting other
    graph-like data structures into Dot graphs (the "Data.GraphViz"
    module provides some functions for this).  It is a structured
    representation of Dot code.

    ["Data.GraphViz.Types.Generalised"] matches the actual structure
    of Dot code.  As such, it is suited for parsing in existing Dot
    code.

    ["Data.GraphViz.Types.Graph"] provides graph operations for
    manipulating Dot graphs; this is suited when you want to edit
    existing Dot code.  It uses generalised Dot graphs for parsing and
    canonical Dot graphs for printing.

    ["Data.GraphViz.Types.Monadic"] is a much easier representation to
    use when defining relatively static Dot graphs in Haskell code,
    and looks vaguely like actual Dot code if you squint a bit.

    Please also read the limitations section at the end for advice on
    how to properly use these Dot representations.

-}
module Data.GraphViz.Types
       ( DotRepr(..)
       , PrintDot(..)
       , ParseDot(..)
       , PrintDotRepr
       , ParseDotRepr
       , PPDotRepr
         -- * Common sub-types
       , GraphID(..)
       , Number (..)
       , ToGraphID(..)
       , textGraphID
       , GlobalAttributes(..)
       , DotNode(..)
       , DotEdge(..)
         -- * Helper types for looking up information within a @DotRepr@.
       , ClusterLookup
       , NodeLookup
       , Path
       , graphStructureInformationClean
       , nodeInformationClean
       , edgeInformationClean
         -- * Obtaining the @DotNode@s and @DotEdges@.
       , graphNodes
       , graphEdges
         -- * Printing and parsing a @DotRepr@.
       , printDotGraph
       , parseDotGraph
       , parseDotGraphLiberally
         -- * Limitations and documentation
         -- $limitations
       ) where

import Data.GraphViz.Attributes.Complete   (rmUnwantedAttributes,
                                            usedByClusters, usedByEdges,
                                            usedByGraphs, usedByNodes)
import Data.GraphViz.Internal.State        (GraphvizState)
import Data.GraphViz.Internal.Util         (bool)
import Data.GraphViz.Parsing               (ParseDot(..), adjustErr,
                                            checkValidParseWithRest, parse,
                                            parseLiberally, runParserWith)
import Data.GraphViz.PreProcessing         (preProcess)
import Data.GraphViz.Printing              (PrintDot(..), printIt)
import Data.GraphViz.Types.Canonical       (DotGraph(..), DotStatements(..),
                                            DotSubGraph(..))
import Data.GraphViz.Types.Internal.Common (DotEdge(..), DotNode(..),
                                            GlobalAttributes(..), GraphID(..),
                                            Number(..), numericValue, withGlob)
import Data.GraphViz.Types.State

import           Control.Arrow       (second, (***))
import           Control.Monad.State (evalState, execState, get, modify, put)
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T

-- -----------------------------------------------------------------------------

-- | This class is used to provide a common interface to different
--   ways of representing a graph in /Dot/ form.
--
--   You will most probably /not/ need to create your own instances of
--   this class.
--
--   The type variable represents the current node type of the Dot
--   graph, and the 'Ord' restriction is there because in practice
--   most implementations of some of these methods require it.
class (Ord n) => DotRepr dg n where
  -- | Convert from a graph in canonical form.  This is especially
  --   useful when using the functions from "Data.GraphViz.Algorithms".
  --
  --   See @FromGeneralisedDot@ in "Data.GraphViz.Types.Generalised"
  --   for a semi-inverse of this function.
  fromCanonical :: DotGraph n -> dg n

  -- | Return the ID of the graph.
  getID :: dg n -> Maybe GraphID

  -- | Set the ID of the graph.
  setID :: GraphID -> dg n -> dg n

  -- | Is this graph directed?
  graphIsDirected :: dg n -> Bool

  -- | Set whether a graph is directed or not.
  setIsDirected :: Bool -> dg n -> dg n

  -- | Is this graph strict?  Strict graphs disallow multiple edges.
  graphIsStrict :: dg n -> Bool

  -- | A strict graph disallows multiple edges.
  setStrictness :: Bool -> dg n -> dg n

  -- | Change the node values.  This function is assumed to be
  --   /injective/, otherwise the resulting graph will not be
  --   identical to the original (modulo labels).
  mapDotGraph :: (DotRepr dg n') => (n -> n') -> dg n -> dg n'

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

  -- | Give any anonymous sub-graphs or clusters a unique identifier
  --   (i.e. there will be no 'Nothing' key in the 'ClusterLookup'
  --   from 'graphStructureInformation').
  unAnonymise :: dg n -> dg n

-- | A variant of 'graphStructureInformation' with default attributes
--   removed and only attributes usable by graph/cluster kept (where
--   applicable).
graphStructureInformationClean :: (DotRepr dg n) => dg n
                                  -> (GlobalAttributes, ClusterLookup)
graphStructureInformationClean = (globOnly *** fmap (second clustOnly))
                                 . graphStructureInformation
  where
    globOnly = withGlob $ filter usedByGraphs . rmUnwantedAttributes

    clustOnly = withGlob $ filter usedByClusters . rmUnwantedAttributes


-- | A variant of 'nodeInformation' with default attributes removed
--   and only attributes used by nodes kept.
nodeInformationClean :: (DotRepr dg n) => Bool -> dg n -> NodeLookup n
nodeInformationClean = (fmap (second nodeOnly) .) . nodeInformation
  where
    nodeOnly = filter usedByNodes . rmUnwantedAttributes

-- | A variant of 'edgeInformation' with default attributes removed
--   and only attributes used by edges kept.
edgeInformationClean :: (DotRepr dg n) => Bool -> dg n -> [DotEdge n]
edgeInformationClean = (map rmEdgeAs .) . edgeInformation
  where
    rmEdgeAs de = de { edgeAttributes = edgeOnly $ edgeAttributes de }

    edgeOnly = filter usedByEdges . rmUnwantedAttributes


-- | This class exists just to make type signatures nicer; all
--   instances of 'DotRepr' should also be an instance of
--   'PrintDotRepr'.
class (DotRepr dg n, PrintDot (dg n)) => PrintDotRepr dg n

-- | This class exists just to make type signatures nicer; all
--   instances of 'DotRepr' should also be an instance of
--   'ParseDotRepr'.
class (DotRepr dg n, ParseDot (dg n)) => ParseDotRepr dg n

-- | This class exists just to make type signatures nicer; all
--   instances of 'DotRepr' should also be an instance of
--   'PPDotRepr'.
class (PrintDotRepr dg n, ParseDotRepr dg n) => PPDotRepr dg n

-- | Returns all resultant 'DotNode's in the 'DotRepr' (not including
--   'NodeAttr's).
graphNodes :: (DotRepr dg n) => dg n -> [DotNode n]
graphNodes = toDotNodes . nodeInformation False

-- | Returns all resultant 'DotEdge's in the 'DotRepr' (not including
--   'EdgeAttr's).
graphEdges :: (DotRepr dg n) => dg n -> [DotEdge n]
graphEdges = edgeInformation False

-- | The actual /Dot/ code for an instance of 'DotRepr'.  Note that it
--   is expected that @'parseDotGraph' . 'printDotGraph' == 'id'@
--   (this might not be true the other way around due to un-parseable
--   components).
printDotGraph :: (PrintDotRepr dg n) => dg n -> Text
printDotGraph = printIt

-- | Parse a limited subset of the Dot language to form an instance of
--   'DotRepr'.  Each instance may have its own limitations on what
--   may or may not be parseable Dot code.
--
--   Also removes any comments, etc. before parsing.
parseDotGraph :: (ParseDotRepr dg n) => Text -> dg n
parseDotGraph = parseDotGraphWith id

-- | As with 'parseDotGraph', but if an 'Attribute' cannot be parsed
--   strictly according to the known rules, let it fall back to being
--   parsed as an 'UnknownAttribute'.  This is especially useful for
--   when using a version of Graphviz that is either newer (especially
--   for the XDot attributes) or older (when some attributes have
--   changed) but you'd still prefer it to parse rather than throwing
--   an error.
parseDotGraphLiberally :: (ParseDotRepr dg n) => Text -> dg n
parseDotGraphLiberally = parseDotGraphWith parseLiberally

parseDotGraphWith :: (ParseDotRepr dg n) => (GraphvizState -> GraphvizState)
                     -> Text -> dg n
parseDotGraphWith f = prs . preProcess
  where
    prs = checkValidParseWithRest . runParserWith f parse'

    parse' = parse `adjustErr`
             ("Unable to parse the Dot graph; usually this is because of either:\n\
              \  * Wrong choice of representation: try the Generalised one\n\
              \  * Wrong choice of node type; try with `DotGraph String`.\n\
              \\n\
              \The actual parsing error was:\n\t"++)

-- -----------------------------------------------------------------------------
-- Instance for Canonical graphs, to avoid cyclic modules.

instance (Ord n) => DotRepr DotGraph n where
  fromCanonical = id

  getID = graphID

  setID i g = g { graphID = Just i }

  graphIsDirected = directedGraph

  setIsDirected d g = g { directedGraph = d }

  graphIsStrict = strictGraph

  setStrictness s g = g { strictGraph = s }

  mapDotGraph = fmap

  graphStructureInformation = getGraphInfo
                              . statementStructure . graphStatements

  nodeInformation wGlobal = getNodeLookup wGlobal
                            . statementNodes . graphStatements

  edgeInformation wGlobal = getDotEdges wGlobal
                            . statementEdges . graphStatements

  unAnonymise = renumber

instance (Ord n, PrintDot n) => PrintDotRepr DotGraph n
instance (Ord n, ParseDot n) => ParseDotRepr DotGraph n
instance (Ord n, PrintDot n, ParseDot n) => PPDotRepr DotGraph n

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

renumber    :: DotGraph n -> DotGraph n
renumber dg = dg { graphStatements = newStmts }
  where
    startN = succ $ maxSGInt dg

    newStmts = evalState (stRe $ graphStatements dg) startN

    stRe st = do sgs' <- mapM sgRe $ subGraphs st
                 return $ st { subGraphs = sgs' }
    sgRe sg = do sgid' <- case subGraphID sg of
                            Nothing -> do n <- get
                                          put $ succ n
                                          return . Just . Num $ Int n
                            sgid    -> return sgid
                 stmts' <- stRe $ subGraphStmts sg
                 return $ sg { subGraphID    = sgid'
                             , subGraphStmts = stmts'
                             }

maxSGInt    :: DotGraph n -> Int
maxSGInt dg = execState (stInt $ graphStatements dg)
              . (`check` 0)
              $ graphID dg
  where
    check = maybe id max . (numericValue =<<)

    stInt = mapM_ sgInt . subGraphs
    sgInt sg = do modify (check $ subGraphID sg)
                  stInt $ subGraphStmts sg

-- -----------------------------------------------------------------------------

-- | A convenience class to make it easier to convert data types to
--   'GraphID' values, e.g. for cluster identifiers.
--
--   In most cases, conversion would be via the 'Text' or 'String'
--   instances (e.g. using 'show').
class ToGraphID a where
  toGraphID :: a -> GraphID

-- | An alias for 'toGraphID' for use with the @OverloadedStrings@
--   extension.
textGraphID :: Text -> GraphID
textGraphID = toGraphID

instance ToGraphID Text where
  toGraphID = Str

instance ToGraphID String where
  toGraphID = toGraphID . T.pack

instance ToGraphID Char where
  toGraphID = toGraphID . T.singleton

instance ToGraphID Int where
  toGraphID = Num . Int

-- | This instance loses precision by going via 'Int'.
instance ToGraphID Integer where
  toGraphID = Num . Int . fromInteger

instance ToGraphID Double where
  toGraphID = Num . Dbl

-- -----------------------------------------------------------------------------

{- $limitations

   Printing of /Dot/ code is done as strictly as possible, whilst
   parsing is as permissive as possible.  For example, if the types
   allow it then @\"2\"@ will be parsed as an 'Int' value.  Note that
   quoting and escaping of textual values is done automagically.

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

   See "Data.GraphViz.Attributes.Complete" for more limitations.
 -}
