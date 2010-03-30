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

   * Cannot create edges with subgraphs\/clusters as one of the
     end points.

   * When either creating a 'DotGraph' by hand or parsing one, it is
     possible to specify that @'directedGraph' = d@, but 'DotEdge'
     values with @'directedEdge' = 'not' d@.

   * Nodes cannot have Port values.

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

   See "Data.GraphViz.Attributes" for more limitations.

-}
module Data.GraphViz.Types
    ( -- * Abstraction from the representation type
      DotRepr(..)
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
    , GlobalAttributes(..)
    , DotSubGraph(..)
    , DotNode(..)
    , DotEdge(..) -- Re-exported from Data.GraphViz.Types.Common
    ) where

import Data.GraphViz.Types.Common
import Data.GraphViz.Attributes( Attributes, Attribute
                               , usedByGraphs, usedByClusters, usedBySubGraphs
                               , usedByNodes, usedByEdges)
import Data.GraphViz.Util
import Data.GraphViz.Parsing
import Data.GraphViz.PreProcessing
import Data.GraphViz.Printing

import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | This class is used to provide a common interface to different
--   ways of representing a graph in /Dot/ form.
class (PrintDot (dg n), ParseDot (dg n)) => DotRepr dg n where
  -- | Is this graph directed?
  graphIsDirected :: dg n -> Bool

  -- | A strict graph disallows multiple edges.
  makeStrict :: dg n -> dg n

  -- | Set the ID of the graph.
  setID :: GraphID -> dg n -> dg n

  -- | Return all the 'DotNode's contained within this 'DotRepr'.
  --   There is no requirement for it to return implicitly defined
  --   nodes found only within a 'DotEdge'.
  graphNodes :: dg n -> [DotNode n]

  -- | Return all the 'DotEdge's contained within this 'DotRepr'.
  graphEdges :: dg n -> [DotEdge n]

-- | The actual /Dot/ code for an instance of 'DotRepr'.  Note that it
--   is expected that @'parseDotGraph' . 'printDotGraph' == 'id'@
--   (this might not be true the other way around due to un-parseable
--   components).
printDotGraph :: (DotRepr dg n) => dg n -> String
printDotGraph = printIt

-- | Parse a limited subset of the Dot language to form an instance of
--   'DotRepr'.  Each instance may have its own limitations on what
--   may or may not be parseable Dot code.
--
--   Also removes any comments, etc. before parsing.
parseDotGraph :: (DotRepr dg n) => String -> dg n
parseDotGraph = fst . parseIt . preProcess

-- -----------------------------------------------------------------------------

-- | The internal representation of a graph in Dot form.
data DotGraph a = DotGraph { strictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements a
                           }
                  deriving (Eq, Ord, Show, Read)

instance (PrintDot n, ParseDot n) => DotRepr DotGraph n where
  graphIsDirected = directedGraph

  makeStrict g = g { strictGraph = True }

  setID i g = g { graphID = Just i }

  graphNodes = statementNodes . graphStatements

  graphEdges = statementEdges . graphStatements

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

-- | Used to record invalid 'Attribute' usage.  A 'Just' value denotes
--   that it was used in an explicit 'DotNode' or 'DotEdge' usage;
--   'Nothing' means that it was used in a 'GlobalAttributes' value.
data DotError a = GraphError Attribute
                | NodeError (Maybe a) Attribute
                | EdgeError (Maybe (a,a)) Attribute
                deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------

data DotStatements a = DotStmts { attrStmts :: [GlobalAttributes]
                                , subGraphs :: [DotSubGraph a]
                                , nodeStmts :: [DotNode a]
                                , edgeStmts :: [DotEdge a]
                                }
                     deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotStatements a) where
    unqtDot stmts = vcat [ unqtDot $ attrStmts stmts
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

statementNodes       :: DotStatements a -> [DotNode a]
statementNodes stmts = concatMap subGraphNodes (subGraphs stmts)
                       ++ nodeStmts stmts

statementEdges       :: DotStatements a -> [DotEdge a]
statementEdges stmts = concatMap subGraphEdges (subGraphs stmts)
                       ++ edgeStmts stmts

-- -----------------------------------------------------------------------------

-- | Represents a list of top-level list of 'Attribute's for the
--   entire graph/sub-graph.  Note that 'GraphAttrs' also applies to
--   'DotSubGraph's.
--
--   Note that Dot allows a single 'Attribute' to be listen on a line;
--   if this is the case then when parsing, the type of 'Attribute' it
--   is determined and that type of 'GlobalAttribute' is created.
data GlobalAttributes = GraphAttrs { attrs :: Attributes }
                      | NodeAttrs  { attrs :: Attributes }
                      | EdgeAttrs  { attrs :: Attributes }
                        deriving (Eq, Ord, Show, Read)

instance PrintDot GlobalAttributes where
    unqtDot = printAttrBased printGlobAttrType attrs

    unqtListToDot = printAttrBasedList printGlobAttrType attrs

    listToDot = unqtListToDot

printGlobAttrType              :: GlobalAttributes -> DotCode
printGlobAttrType GraphAttrs{} = text "graph"
printGlobAttrType NodeAttrs{}  = text "node"
printGlobAttrType EdgeAttrs{}  = text "edge"

instance ParseDot GlobalAttributes where
    parseUnqt = parseAttrBased parseGlobAttrType
                `onFail`
                liftM determineType parse

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid listing of global attributes")

    -- Have to do this manually because of the special case
    parseUnqtList = sepBy (whitespace' >> parse) statementEnd
                    `discard`
                    optional statementEnd

    parseList = parseUnqtList

parseGlobAttrType :: Parse (Attributes -> GlobalAttributes)
parseGlobAttrType = oneOf [ stringRep GraphAttrs "graph"
                          , stringRep NodeAttrs "node"
                          , stringRep EdgeAttrs "edge"
                          ]

determineType :: Attribute -> GlobalAttributes
determineType attr
    | usedByGraphs attr   = GraphAttrs attr'
    | usedByClusters attr = GraphAttrs attr' -- Also covers SubGraph case
    | usedByNodes attr    = NodeAttrs attr'
    | otherwise           = EdgeAttrs attr' -- Must be for edges.
    where
      attr' = [attr]

invalidGlobal                   :: (Attribute -> Bool) -> GlobalAttributes
                                   -> [DotError a]
invalidGlobal f (GraphAttrs as) = map GraphError $ filter (not . f) as
invalidGlobal _ (NodeAttrs  as) = map (NodeError Nothing)
                                  $ filter (not . usedByNodes) as
invalidGlobal _ (EdgeAttrs  as) = map (EdgeError Nothing)
                                  $ filter (not . usedByEdges) as

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
printSubGraphID' = printSubGraphID (\sg -> (isCluster sg, subGraphID sg))

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

subGraphNodes :: DotSubGraph a -> [DotNode a]
subGraphNodes = statementNodes . subGraphStmts

subGraphEdges :: DotSubGraph a -> [DotEdge a]
subGraphEdges = statementEdges . subGraphStmts

-- -----------------------------------------------------------------------------

-- | A node in 'DotGraph'.
data DotNode a = DotNode { nodeID :: a
                         , nodeAttributes :: Attributes
                         }
                 deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotNode a) where
    unqtDot = printAttrBased printNodeID nodeAttributes

    unqtListToDot = printAttrBasedList printNodeID nodeAttributes

    listToDot = unqtListToDot

printNodeID :: (PrintDot a) => DotNode a -> DotCode
printNodeID = toDot . nodeID

instance (ParseDot a) => ParseDot (DotNode a) where
    parseUnqt = parseAttrBased parseNodeID

    parse = parseUnqt -- Don't want the option of quoting

    parseUnqtList = parseAttrBasedList parseNodeID

    parseList = parseUnqtList

parseNodeID :: (ParseDot a) => Parse (Attributes -> DotNode a)
parseNodeID = liftM DotNode parseAndCheck
  where
    parseAndCheck = do a <- parse
                       me <- optional $ whitespace >> parseEdgeType
                       maybe (return a) (const notANode) me
    notANode = fail "This appears to be an edge, not a node"

instance Functor DotNode where
    fmap f n = n { nodeID = f $ nodeID n }

invalidNode   :: DotNode a -> [DotError a]
invalidNode n = map (NodeError (Just $ nodeID n))
                $ filter (not . usedByNodes) (nodeAttributes n)

-- -----------------------------------------------------------------------------

-- Defined here rather than in Common with the rest of the Edge stuff
-- because all the DotError stuff is here.

invalidEdge   :: DotEdge a -> [DotError a]
invalidEdge e = map (EdgeError eID)
                $ filter (not . usedByEdges) (edgeAttributes e)
    where
      eID = Just (edgeFromNodeID e, edgeToNodeID e)
