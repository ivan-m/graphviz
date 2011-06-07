{-# LANGUAGE   MultiParamTypeClasses
             , FlexibleInstances
  #-}

{- |
   Module      : Data.GraphViz.Types
   Description : Definition of the Graphviz types.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the overall types and methods that interact
   with them for the Graphviz library.  The specifications are based
   loosely upon the information available at:
     <http://graphviz.org/doc/info/lang.html>

   Printing of /Dot/ code is done as strictly as possible, whilst
   parsing is as permissive as possible.  For example, if the types
   allow it then @\"2\"@ will be parsed as an 'Int' value.  Note that
   quoting and escaping of 'String' values is done automagically.

   A summary of known limitations\/differences:

   * When creating 'GraphID' values for 'graphID' and 'subGraphID',
     you should ensure that none of them have the same printed value
     as one of the 'nodeID' values to avoid any possible problems.

   * If you want any 'GlobalAttributes' in a 'DotSubGraph' and want
     them to only apply to that 'DotSubGraph', then you must ensure it
     does indeed have a valid 'GraphID'.

   * All 'DotSubGraph's with @'isCluster' = 'True'@ /must/ have unique
     'subGraphID' values (well, only if you want them to be generated
     sensibly).

   * If eventually outputting to a format such as SVG, then you should
     make sure that your 'DotGraph' has a 'graphID', as that is used
     as the title of the resulting image.

   * Whilst 'DotGraph', etc. are polymorphic in their node type, you
     should ensure that you use a relatively simple node type (that
     is, it only covers a single line, etc.).

   * Also, whilst Graphviz allows you to mix the types used for nodes,
     this library requires\/assumes that they are all the same type.

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

   * Cannot place items in an arbitrary order: in particular, this
     means that it is not possible to use the normal Graphviz hack of
     having graph attributes that do not apply to subgraphs\/clusters by
     listing them /after/ the subgraphs\/clusters.  If you wish to be able
     to use an arbitrary ordering, you may wish to use
     "Data.GraphViz.Types.Generalised".

   * The parser will strip out comments and pre-processor lines, join
     together multiline statements and concatenate split strings together.
     However, pre-processing within HTML-like labels is currently not
     supported.

   * Graphviz allows a node to be \"defined\" twice (e.g. the actual
     node definition, and then in a subgraph with extra global attributes
     applied to it).  This actually represents the same node, but when
     parsing they will be considered as separate 'DotNode's (such that
     'graphNodes' will return both \"definitions\").  The canonical form
     (see @prettyPrint@ in "Data.GraphViz") combines the \"definitions\"
     into one.

   See "Data.GraphViz.Attributes" for more limitations.

-}
module Data.GraphViz.Types
    ( -- * Abstraction from the representation type
      DotRepr(..)
      -- ** Helper types for looking up information within a @DotRepr@.
    , ClusterLookup
    , NodeLookup
    , Path
      -- ** Obtaining the @DotNode@s and @DotEdges@.
    , graphNodes
    , graphEdges
      -- ** Printing and parsing a @DotRepr@.
    , printDotGraph
    , parseDotGraph
      -- * The overall representation of a graph in /Dot/ format.
    , DotGraph(..)
      -- ** Reporting of errors in a @DotGraph@.
    , DotError(..)
    , isValidGraph
    , graphErrors
      -- * Sub-components of a @DotGraph@.
    , GraphID(..) -- Re-exported from Data.GraphViz.Types.Common
    , DotStatements(..)
    , GlobalAttributes(..) -- Re-exported from Data.GraphViz.Types.Common
    , DotSubGraph(..)
    , DotNode(..)
    , DotEdge(..) -- Re-exported from Data.GraphViz.Types.Common
    ) where

import Data.GraphViz.Types.Common
import Data.GraphViz.Types.State
import Data.GraphViz.Attributes( Attribute
                               , usedByGraphs, usedByClusters, usedBySubGraphs)
import Data.GraphViz.Util
import Data.GraphViz.Parsing
import Data.GraphViz.PreProcessing
import Data.GraphViz.Printing

import Data.Text.Lazy(Text)
import Control.Arrow((&&&))
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | This class is used to provide a common interface to different
--   ways of representing a graph in /Dot/ form.
class (PrintDot (dg n), ParseDot (dg n)) => DotRepr dg n where
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
graphNodes = toDotNodes . nodeInformation True

-- | Returns all resultant 'DotEdge's in the 'DotRepr' (not including
--   'EdgeAttr's).
graphEdges :: (DotRepr dg n) => dg n -> [DotEdge n]
graphEdges = edgeInformation True

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

-- | The internal representation of a graph in Dot form.
data DotGraph a = DotGraph { strictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements a
                           }
                deriving (Eq, Ord, Show, Read)

instance (Ord n, PrintDot n, ParseDot n) => DotRepr DotGraph n where
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

-- | Check if all the 'Attribute's are being used correctly.
isValidGraph :: DotGraph a -> Bool
isValidGraph = null . graphErrors

-- | Return detectable errors in the 'DotGraph'.
graphErrors :: DotGraph a -> [DotError a]
graphErrors = invalidStmts usedByGraphs . graphStatements

instance (PrintDot a) => PrintDot (DotGraph a) where
  unqtDot = printStmtBased printGraphID' graphStatements toDot
    where
      printGraphID' = printGraphID strictGraph directedGraph graphID

instance (ParseDot a) => ParseDot (DotGraph a) where
  parseUnqt = parseStmtBased parse (parseGraphID DotGraph)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "\n\nNot a valid DotGraph")

instance Functor DotGraph where
  fmap f g = g { graphStatements = fmap f $ graphStatements g }

-- -----------------------------------------------------------------------------

data DotStatements a = DotStmts { attrStmts :: [GlobalAttributes]
                                , subGraphs :: [DotSubGraph a]
                                , nodeStmts :: [DotNode a]
                                , edgeStmts :: [DotEdge a]
                                }
                     deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotStatements a) where
  unqtDot stmts = vcat $ sequence [ unqtDot $ attrStmts stmts
                                  , unqtDot $ subGraphs stmts
                                  , unqtDot $ nodeStmts stmts
                                  , unqtDot $ edgeStmts stmts
                                  ]

instance (ParseDot a) => ParseDot (DotStatements a) where
  parseUnqt = do atts <- tryParseList
                 newline'
                 sGraphs <- tryParseList
                 newline'
                 nodes <- tryParseList
                 newline'
                 edges <- tryParseList
                 return $ DotStmts atts sGraphs nodes edges

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "Not a valid set of statements")

instance Functor DotStatements where
  fmap f stmts = stmts { subGraphs = map (fmap f) $ subGraphs stmts
                       , nodeStmts = map (fmap f) $ nodeStmts stmts
                       , edgeStmts = map (fmap f) $ edgeStmts stmts
                       }

-- | The function represents which function to use to check the
--   'GraphAttrs' values.
invalidStmts         :: (Attribute -> Bool) -> DotStatements a -> [DotError a]
invalidStmts f stmts = concatMap (invalidGlobal f) (attrStmts stmts)
                       ++ concatMap invalidSubGraph (subGraphs stmts)
                       ++ concatMap invalidNode (nodeStmts stmts)
                       ++ concatMap invalidEdge (edgeStmts stmts)

statementStructure :: DotStatements n -> GraphState ()
statementStructure stmts
  = do mapM_ addGraphGlobals $ attrStmts stmts
       mapM_ (withSubGraphID addSubGraph statementStructure) $ subGraphs stmts

statementNodes :: (Ord a) => DotStatements a -> NodeState a ()
statementNodes stmts
  = do mapM_ addNodeGlobals $ attrStmts stmts
       mapM_ (withSubGraphID recursiveCall statementNodes) $ subGraphs stmts
       mapM_ addNode $ nodeStmts stmts
       mapM_ addEdgeNodes $ edgeStmts stmts

statementEdges :: DotStatements a -> EdgeState a ()
statementEdges stmts
  = do mapM_ addEdgeGlobals $ attrStmts stmts
       mapM_ (withSubGraphID recursiveCall statementEdges) $ subGraphs stmts
       mapM_ addEdge $ edgeStmts stmts

-- -----------------------------------------------------------------------------

data DotSubGraph a = DotSG { isCluster     :: Bool
                           , subGraphID    :: Maybe GraphID
                           , subGraphStmts :: DotStatements a
                           }
                   deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotSubGraph a) where
  unqtDot = printStmtBased printSubGraphID' subGraphStmts toDot

  unqtListToDot = printStmtBasedList printSubGraphID' subGraphStmts toDot

  listToDot = unqtListToDot

printSubGraphID' :: DotSubGraph a -> DotCode
printSubGraphID' = printSubGraphID (isCluster &&& subGraphID)

instance (ParseDot a) => ParseDot (DotSubGraph a) where
  parseUnqt = parseStmtBased parseUnqt (parseSubGraphID DotSG)
              `onFail`
              -- Take "anonymous" DotSubGraphs into account.
              liftM (DotSG False Nothing) (parseBracesBased parseUnqt)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "\n\nNot a valid Sub Graph")

  parseUnqtList = sepBy (whitespace' >> parseUnqt) newline'

  parseList = parseUnqtList

instance Functor DotSubGraph where
  fmap f sg = sg { subGraphStmts = fmap f $ subGraphStmts sg }

invalidSubGraph    :: DotSubGraph a -> [DotError a]
invalidSubGraph sg = invalidStmts valFunc (subGraphStmts sg)
  where
    valFunc = bool usedBySubGraphs usedByClusters (isCluster sg)

withSubGraphID        :: (Maybe (Maybe GraphID) -> b -> a)
                         -> (DotStatements n -> b) -> DotSubGraph n -> a
withSubGraphID f g sg = f mid . g $ subGraphStmts sg
  where
    mid = bool Nothing (Just $ subGraphID sg) $ isCluster sg
