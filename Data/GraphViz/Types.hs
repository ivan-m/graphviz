{- |
   Module      : Data.GraphViz.Types
   Description : Definition of the GraphViz types.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the overall types and methods that interact
   with them for the GraphViz library.  The specifications are based
   loosely upon the information available at:
     <http://graphviz.org/doc/info/lang.html>

   A summary of known limitations\/differences:

   * Cannot create edges with subgraphs\/clusters as one of the
     end points.

   * Nodes cannot have Port values.

   * Cannot place items in an arbitrary order: in particular, this
     means that it is not possible to use the normal GraphViz hack of
     having graph attributes that do not apply to subgraphs\/clusters
     by listing them /after/ the subgraphs\/clusters.

   * The parser cannot as yet parse comments (in that it doesn't even
     know what comments are, and will throw an error).  Multiline
     statements and C pre-processor lines (that is, lines beginning
     with a @#@) are also not parseable.

   See "Data.GraphViz.Attributes" for more limitations.

-}
module Data.GraphViz.Types
    ( -- * The overall representation of a graph in /Dot/ format.
      DotGraph(..)
      -- ** Printing and parsing a 'DotGraph'.
    , printDotGraph
    , parseDotGraph
      -- ** Functions acting on a 'DotGraph'.
    , setID
    , makeStrict
    , graphNodes
    , graphEdges
    , isValidGraph
    , graphErrors
      -- * Sub-components of a 'DotGraph'.
    , GraphID(..)
    , DotStatements(..)
    , GlobalAttributes(..)
    , DotSubGraph(..)
    , DotNode(..)
    , DotEdge(..)
    ) where

import Data.GraphViz.Attributes
import Data.GraphViz.Types.Internal
import Data.GraphViz.Types.Parsing
import Data.GraphViz.Types.Printing

import Data.Maybe(isJust)
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | The internal representation of a graph in Dot form.
data DotGraph = DotGraph { strictGraph     :: Bool
                         , directedGraph   :: Bool
                         , graphID         :: Maybe GraphID
                         , graphStatements :: DotStatements
                         }
                deriving (Eq, Show, Read)

-- | A strict graph disallows multiple edges.
makeStrict   :: DotGraph -> DotGraph
makeStrict g = g { strictGraph = True }

-- | Set the ID of the graph.
setID     :: GraphID -> DotGraph -> DotGraph
setID i g = g { graphID = Just i }

-- | Return all the 'DotNode's contained within this 'DotGraph'.  Note
--   that it does not yet return all implicitly defined nodes
--   contained only within 'DotEdge's.
graphNodes :: DotGraph -> [DotNode]
graphNodes = statementNodes . graphStatements

-- | Return all the 'DotEdge's contained within this 'DotGraph'.
graphEdges :: DotGraph -> [DotEdge]
graphEdges = statementEdges . graphStatements

-- | The actual /Dot/ code for a 'DotGraph'.  Note that it is expected
--   that @'parseDotGraph' . 'printDotGraph' == 'id'@ (this might not
--   be true the other way around due to un-parseable components).
printDotGraph :: DotGraph -> String
printDotGraph = renderDot . toDot

-- | Parse a limited subset of the Dot language to form a 'DotGraph'
--   (that is, the caveats listed in "Data.GraphViz.Attributes" aside,
--   Dot graphs are parsed if they match the layout of DotGraph).
parseDotGraph :: String -> DotGraph
parseDotGraph = fst . runParser parse

-- | Check if all the @Attribute@s are being used correctly.
isValidGraph :: DotGraph -> Bool
isValidGraph = null . graphErrors

-- | Return detectable errors in the 'DotGraph'.
graphErrors :: DotGraph -> [DotError]
graphErrors = invalidStmts usedByGraphs . graphStatements

instance PrintDot DotGraph where
    unqtDot = printStmtBased printGraphID graphStatements

printGraphID   :: DotGraph -> DotCode
printGraphID g = bool strGraph' empty (strictGraph g)
                 <+> bool dirGraph' undirGraph' (directedGraph g)
                 <+> maybe empty toDot (graphID g)

instance ParseDot DotGraph where
    parseUnqt = parseStmtBased parseGraphID

    parse = parseUnqt -- Don't want the option of quoting

parseGraphID :: Parse (DotStatements -> DotGraph)
parseGraphID = do str <- liftM isJust
                         $ optional (parseAndSpace $ string strGraph)
                  dir <- parseAndSpace ( stringRep True dirGraph
                                         `onFail`
                                         stringRep False undirGraph
                                       )
                  gID <- optional parse
                  return $ DotGraph str dir gID

dirGraph :: String
dirGraph = "digraph"

dirGraph' :: DotCode
dirGraph' = text dirGraph

undirGraph :: String
undirGraph = "graph"

undirGraph' :: DotCode
undirGraph' = text undirGraph

strGraph :: String
strGraph = "strict"

strGraph' :: DotCode
strGraph' = text strGraph

-- -----------------------------------------------------------------------------

-- | Used to record invalid 'Attribute' usage.  A 'Just' value denotes
--   that it was used in an explicit 'DotNode' or 'DotEdge' usage;
--   'Nothing' means that it was used in a 'GlobalAttributes' value.
data DotError = GraphError Attribute
              | NodeError (Maybe Int) Attribute
              | EdgeError (Maybe (Int,Int)) Attribute
                deriving (Eq, Show, Read)

-- -----------------------------------------------------------------------------

data GraphID = Str String
             | Int Int
             | Dbl Double
             | HTML URL
               deriving (Eq, Show, Read)

instance PrintDot GraphID where
    unqtDot (Str str) = unqtDot str
    unqtDot (Int i)   = unqtDot i
    unqtDot (Dbl d)   = unqtDot d
    unqtDot (HTML u)  = unqtDot u

    toDot (Str str) = toDot str
    toDot gID       = unqtDot gID

instance ParseDot GraphID where
    parseUnqt = oneOf [ liftM Str parseUnqt
                      , liftM Int parseUnqt
                      , liftM Dbl parseUnqt
                      , liftM HTML parseUnqt
                      ]

    parse = oneOf [ liftM Int parse
                  , liftM Dbl parse
                  , liftM HTML parse
                  -- Parse last so that quoted numbers are parsed as numbers.
                  , liftM Str parse
                  ]

-- -----------------------------------------------------------------------------

data DotStatements = DotStmts { attrStmts :: [GlobalAttributes]
                              , subGraphs :: [DotSubGraph]
                              , nodeStmts :: [DotNode]
                              , edgeStmts :: [DotEdge]
                              }
                     deriving (Eq, Show, Read)

instance PrintDot DotStatements where
    unqtDot stmts = vcat [ toDot $ attrStmts stmts
                         , toDot $ subGraphs stmts
                         , toDot $ nodeStmts stmts
                         , toDot $ edgeStmts stmts
                         ]

instance ParseDot DotStatements where
    parseUnqt = do attrs <- parse
                   newline'
                   subGraphs <- parse
                   newline'
                   nodes <- parse
                   newline'
                   edges <- parse
                   return $ DotStmts attrs subGraphs nodes edges

    parse = parseUnqt -- Don't want the option of quoting

printStmtBased          :: (a -> DotCode) -> (a -> DotStatements)
                           -> a -> DotCode
printStmtBased ff fss a = vcat [ ff a <+> lbrace
                               , indent stmts
                               , rbrace
                               ]
    where
      indent = nest 4
      stmts = toDot $ fss a

printStmtBasedList        :: (a -> DotCode) -> (a -> DotStatements)
                             -> [a] -> DotCode
printStmtBasedList ff fss = vcat . map (printStmtBased ff fss)

parseStmtBased   :: Parse (DotStatements -> a) -> Parse a
parseStmtBased p = do f <- p
                      whitespace'
                      character '{'
                      newline'
                      stmts <- parse
                      newline'
                      whitespace'
                      character '}'
                      return $ f stmts

parseStmtBasedList   :: Parse (DotStatements -> a) -> Parse [a]
parseStmtBasedList p = sepBy (whitespace' >> parseStmtBased p) newline'

-- | The function represents which function to use to check the
--   'GraphAttrs' values.
invalidStmts         :: (Attribute -> Bool) -> DotStatements -> [DotError]
invalidStmts f stmts = concatMap (invalidGlobal f) (attrStmts stmts)
                       ++ concatMap invalidSubGraph (subGraphs stmts)
                       ++ concatMap invalidNode (nodeStmts stmts)
                       ++ concatMap invalidEdge (edgeStmts stmts)

statementNodes       :: DotStatements -> [DotNode]
statementNodes stmts = concatMap subGraphNodes (subGraphs stmts)
                       ++ nodeStmts stmts

statementEdges       :: DotStatements -> [DotEdge]
statementEdges stmts = concatMap subGraphEdges (subGraphs stmts)
                       ++ edgeStmts stmts

-- -----------------------------------------------------------------------------

-- | Represents a list of top-level list of 'Attribute's for the
--   entire graph/sub-graph.
data GlobalAttributes = GraphAttrs { attrs :: Attributes }
                      | NodeAttrs  { attrs :: Attributes }
                      | EdgeAttrs  { attrs :: Attributes }
                        deriving (Eq, Show, Read)

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

    parse = parseUnqt -- Don't want the option of quoting

    parseUnqtList = parseAttrBasedList parseGlobAttrType

    parseList = parseUnqtList

parseGlobAttrType :: Parse (Attributes -> GlobalAttributes)
parseGlobAttrType = oneOf [ stringRep GraphAttrs "graph"
                          , stringRep NodeAttrs "node"
                          , stringRep EdgeAttrs "edge"
                          ]

invalidGlobal                   :: (Attribute -> Bool) -> GlobalAttributes
                                   -> [DotError]
invalidGlobal f (GraphAttrs as) = map GraphError $ filter (not . f) as
invalidGlobal _ (NodeAttrs  as) = map (NodeError Nothing)
                                  $ filter (not . usedByNodes) as
invalidGlobal _ (EdgeAttrs  as) = map (EdgeError Nothing)
                                  $ filter (not . usedByEdges) as

-- -----------------------------------------------------------------------------

data DotSubGraph = DotSG { isCluster     :: Bool
                         , subGraphID    :: Maybe GraphID
                         , subGraphStmts :: DotStatements
                         }
                   deriving (Eq, Show, Read)

instance PrintDot DotSubGraph where
    unqtDot = printStmtBased printSubGraphID subGraphStmts

    unqtListToDot = printStmtBasedList printSubGraphID subGraphStmts

    listToDot = unqtListToDot

printSubGraphID   :: DotSubGraph -> DotCode
printSubGraphID s = sGraph'
                    <+> bool clust' empty isCl
                    <+> maybe empty dtID (subGraphID s)
    where
      isCl = isCluster s
      dtID sId = bool (char '_') empty isCl
                 <> toDot sId

instance ParseDot DotSubGraph where
    parseUnqt = parseStmtBased parseSubGraphID

    parse = parseUnqt -- Don't want the option of quoting

    parseUnqtList = parseStmtBasedList parseSubGraphID

    parseList = parseUnqtList

parseSubGraphID :: Parse (DotStatements -> DotSubGraph)
parseSubGraphID = do string sGraph
                     whitespace'
                     isCl <- liftM isJust
                             $ optional (string clust)
                               `discard` character '_'
                     sId <- optional parse
                     return $ DotSG isCl sId

sGraph :: String
sGraph = "subgraph"

sGraph' :: DotCode
sGraph' = text sGraph

clust :: String
clust = "cluster"

clust' :: DotCode
clust' = text clust

invalidSubGraph    :: DotSubGraph -> [DotError]
invalidSubGraph sg = invalidStmts valFunc (subGraphStmts sg)
    where
      valFunc = bool usedByClusters usedBySubGraphs (isCluster sg)

subGraphNodes :: DotSubGraph -> [DotNode]
subGraphNodes = statementNodes . subGraphStmts

subGraphEdges :: DotSubGraph -> [DotEdge]
subGraphEdges = statementEdges . subGraphStmts

-- -----------------------------------------------------------------------------

-- | A node in 'DotGraph' is either a singular node, or a cluster
--   containing nodes (or more clusters) within it.
--   At the moment, clusters are not parsed.
data DotNode = DotNode { nodeID :: Int
                       , nodeAttributes :: Attributes
                       }
               deriving (Eq, Show, Read)

instance PrintDot DotNode where
    unqtDot = printAttrBased printNodeID nodeAttributes

    unqtListToDot = printAttrBasedList printNodeID nodeAttributes

    listToDot = unqtListToDot

printNodeID :: DotNode -> DotCode
printNodeID = unqtDot . nodeID

instance ParseDot DotNode where
    parseUnqt = parseAttrBased parseNodeID

    parse = parseUnqt -- Don't want the option of quoting

    parseUnqtList = parseAttrBasedList parseNodeID

    parseList = parseUnqtList

parseNodeID :: Parse (Attributes -> DotNode)
parseNodeID = liftM DotNode parseUnqt

invalidNode   :: DotNode -> [DotError]
invalidNode n = map (NodeError (Just $ nodeID n))
                $ filter (not . usedByNodes) (nodeAttributes n)

-- -----------------------------------------------------------------------------

-- | An edge in 'DotGraph'.
data DotEdge = DotEdge { edgeHeadNodeID :: Int
                       , edgeTailNodeID :: Int
                       , directedEdge   :: Bool
                       , edgeAttributes :: Attributes
                       }
             deriving (Eq, Show, Read)

instance PrintDot DotEdge where
    unqtDot = printAttrBased printEdgeID edgeAttributes

    unqtListToDot = printAttrBasedList printEdgeID edgeAttributes

    listToDot = unqtListToDot

printEdgeID   :: DotEdge -> DotCode
printEdgeID e = unqtDot (edgeHeadNodeID e)
                <+> bool dirEdge' undirEdge' (directedEdge e)
                <+> unqtDot (edgeTailNodeID e)


instance ParseDot DotEdge where
    parseUnqt = parseAttrBased parseEdgeID

    parse = parseUnqt -- Don't want the option of quoting

    parseUnqtList = parseAttrBasedList parseEdgeID

    parseList = parseUnqtList

parseEdgeID :: Parse (Attributes -> DotEdge)
parseEdgeID = do eHead <- parse
                 whitespace'
                 edgeType <- strings [dirEdge, undirEdge]
                 let eType = edgeType == dirEdge
                 whitespace'
                 eTail <- parse
                 return $ DotEdge eHead eTail eType

invalidEdgeAttributes   :: DotEdge -> [(DotEdge, Attribute)]
invalidEdgeAttributes e = map ((,) e)
                          . filter (not . usedByEdges)
                          $ edgeAttributes e

dirEdge :: String
dirEdge = "->"

dirEdge' :: DotCode
dirEdge' = text dirEdge

undirEdge :: String
undirEdge = "--"

undirEdge' :: DotCode
undirEdge' = text undirEdge

invalidEdge   :: DotEdge -> [DotError]
invalidEdge e = map (EdgeError eID)
                $ filter (not . usedByEdges) (edgeAttributes e)
    where
      eID = Just (edgeHeadNodeID e, edgeTailNodeID e)

-- -----------------------------------------------------------------------------

-- Printing and parsing helpers.

printAttrBased          :: (a -> DotCode) -> (a -> Attributes) -> a -> DotCode
printAttrBased ff fas a = dc <> semi
    where
      f = ff a
      dc = case fas a of
             [] -> f
             as -> f <+> toDot as

printAttrBasedList        :: (a -> DotCode) -> (a -> Attributes)
                             -> [a] -> DotCode
printAttrBasedList ff fas = vcat . map (printAttrBased ff fas)

parseAttrBased   :: Parse (Attributes -> a) -> Parse a
parseAttrBased p = do f <- p
                      whitespace'
                      attrs <- tryParseList
                      whitespace'
                      character ';'
                      return $ f attrs

parseAttrBasedList   :: Parse (Attributes -> a) -> Parse [a]
parseAttrBasedList p = sepBy (whitespace' >> parseAttrBased p) newline'
