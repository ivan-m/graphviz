{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Types.Common
   Description : Common internal functions for dealing with overall types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides common functions used by both
   "Data.GraphViz.Types" as well as "Data.GraphViz.Types.Generalised".
-}
module Data.GraphViz.Types.Common where

import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Util
import Data.GraphViz.Attributes( Attributes, Attribute(HeadPort, TailPort)
                               , usedByGraphs, usedByClusters
                               , usedByNodes, usedByEdges)
import Data.GraphViz.Attributes.Internal(PortPos, parseEdgeBasedPP)

import Data.Maybe(isJust)
import qualified Data.Text.Lazy as Text
import Control.Monad(liftM, liftM2, when)

-- -----------------------------------------------------------------------------
-- This is re-exported by Data.GraphViz.Types

-- | A polymorphic type that covers all possible ID values allowed by
--   Dot syntax.  Note that whilst the 'ParseDot' and 'PrintDot'
--   instances for 'String' will properly take care of the special
--   cases for numbers, they are treated differently here.
data GraphID = Str String
             | Int Int
             | Dbl Double
               deriving (Eq, Ord, Show, Read)

instance PrintDot GraphID where
    unqtDot (Str str) = unqtDot str
    unqtDot (Int i)   = unqtDot i
    unqtDot (Dbl d)   = unqtDot d

    toDot (Str str) = toDot str
    toDot gID       = unqtDot gID

instance ParseDot GraphID where
    parseUnqt = liftM stringNum parseUnqt

    parse = liftM stringNum parse
            `adjustErr`
            (++ "\nNot a valid GraphID")

stringNum     :: String -> GraphID
stringNum str = maybe checkDbl Int $ stringToInt str
  where
    checkDbl = if isNumString str
               then Dbl $ toDouble str
               else Str str

-- -----------------------------------------------------------------------------

-- | Used to record invalid 'Attribute' usage.  A 'Just' value denotes
--   that it was used in an explicit 'DotNode' or 'DotEdge' usage;
--   'Nothing' means that it was used in a 'GlobalAttributes' value.
data DotError a = GraphError Attribute
                | NodeError (Maybe a) Attribute
                | EdgeError (Maybe (a,a)) Attribute
                deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------------------

-- Re-exported by Data.GraphViz.Types and Data.GraphViz.Types.Generalised

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
    -- Can't use printAttrBased because an empty list still must be printed.
    unqtDot ga = printGlobAttrType ga <+> toDot (attrs ga) <> semi

    unqtListToDot = printAttrBasedList printGlobAttrType attrs

    listToDot = unqtListToDot

printGlobAttrType              :: GlobalAttributes -> DotCode
printGlobAttrType GraphAttrs{} = text "graph"
printGlobAttrType NodeAttrs{}  = text "node"
printGlobAttrType EdgeAttrs{}  = text "edge"

instance ParseDot GlobalAttributes where
    -- Not using parseAttrBased here because we want to force usage of
    -- Attributes.
    parseUnqt = do gat <- parseGlobAttrType
                   as <- whitespace' >> parse
                   return $ gat as
                `onFail`
                liftM determineType parse

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid listing of global attributes")

    -- Have to do this manually because of the special case
    parseUnqtList = parseStatements parse

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
                       me <- optional parseUnwanted
                       maybe (return a) (const notANode) me
    notANode = fail "This appears to be an edge, not a node"
    parseUnwanted = oneOf [ parseEdgeType >> return ()
                          , character ':' >> return () -- PortPos value
                          ]

instance Functor DotNode where
    fmap f n = n { nodeID = f $ nodeID n }

invalidNode   :: DotNode a -> [DotError a]
invalidNode n = map (NodeError (Just $ nodeID n))
                $ filter (not . usedByNodes) (nodeAttributes n)

-- -----------------------------------------------------------------------------

-- This is re-exported in Data.GraphViz.Types; defined here so that
-- Generalised can access and use parseEdgeLine (needed for "a -> b ->
-- c"-style edge statements).

-- | An edge in 'DotGraph'.
data DotEdge a = DotEdge { edgeFromNodeID :: a
                         , edgeToNodeID   :: a
                         , directedEdge   :: Bool
                         , edgeAttributes :: Attributes
                         }
             deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotEdge a) where
    unqtDot = printAttrBased printEdgeID edgeAttributes

    unqtListToDot = printAttrBasedList printEdgeID edgeAttributes

    listToDot = unqtListToDot

printEdgeID   :: (PrintDot a) => DotEdge a -> DotCode
printEdgeID e = toDot (edgeFromNodeID e)
                <+> bool undirEdge' dirEdge' (directedEdge e)
                <+> toDot (edgeToNodeID e)


instance (ParseDot a) => ParseDot (DotEdge a) where
    parseUnqt = parseAttrBased parseEdgeID

    parse = parseUnqt -- Don't want the option of quoting

    -- Have to take into account edges of the type "n1 -> n2 -> n3", etc.
    parseUnqtList = liftM concat
                    $ parseStatements parseEdgeLine

    parseList = parseUnqtList

parseEdgeID :: (ParseDot a) => Parse (Attributes -> DotEdge a)
parseEdgeID = do eFrom <- parseEdgeNode
                 eType <- parseEdgeType
                 eTo <- parseEdgeNode
                 return $ mkEdge eFrom eType eTo

type EdgeNode a = (a, Maybe PortPos)

-- | Takes into account edge statements containing something like
--   @a -> \{b c\}@.
parseEdgeNodes :: (ParseDot a) => Parse [EdgeNode a]
parseEdgeNodes = parseBraced ( wrapWhitespace
                               -- Should really use sepBy1, but this will do.
                               $ parseStatements parseEdgeNode
                             )
                 `onFail`
                 liftM return parseEdgeNode

parseEdgeNode :: (ParseDot a) => Parse (EdgeNode a)
parseEdgeNode = liftM2 (,) parse
                           (optional $ character ':' >> parseEdgeBasedPP)

mkEdge :: EdgeNode a -> Bool -> EdgeNode a
          -> Attributes -> DotEdge a
mkEdge (eFrom, mFP) eDir (eTo, mTP) = DotEdge eFrom eTo eDir
                                      . addPortPos TailPort mFP
                                      . addPortPos HeadPort mTP

mkEdges :: [EdgeNode a] -> Bool -> [EdgeNode a]
           -> Attributes -> [DotEdge a]
mkEdges fs eDir ts as = liftM2 (\f t -> mkEdge f eDir t as) fs ts

addPortPos   :: (PortPos -> Attribute) -> Maybe PortPos
                -> Attributes -> Attributes
addPortPos c = maybe id ((:) . c)

parseEdgeType :: Parse Bool
parseEdgeType = wrapWhitespace $ stringRep True dirEdge
                                 `onFail`
                                 stringRep False undirEdge

parseEdgeLine :: (ParseDot a) => Parse [DotEdge a]
parseEdgeLine = do n1 <- parseEdgeNodes
                   ens <- many1 $ do eType <- parseEdgeType
                                     n <- parseEdgeNodes
                                     return (eType, n)
                   let ens' = (True, n1) : ens
                       efs = zipWith mkEdg ens' (tail ens')
                       ef = return $ \ as -> concatMap ($as) efs
                   parseAttrBased ef
    where
      mkEdg (_, hn) (et, tn) = mkEdges hn et tn

instance Functor DotEdge where
    fmap f e = e { edgeFromNodeID = f $ edgeFromNodeID e
                 , edgeToNodeID   = f $ edgeToNodeID e
                 }

dirEdge :: String
dirEdge = "->"

dirEdge' :: DotCode
dirEdge' = text dirEdge

undirEdge :: String
undirEdge = "--"

undirEdge' :: DotCode
undirEdge' = text undirEdge

invalidEdge   :: DotEdge a -> [DotError a]
invalidEdge e = map (EdgeError eID)
                $ filter (not . usedByEdges) (edgeAttributes e)
    where
      eID = Just (edgeFromNodeID e, edgeToNodeID e)

-- -----------------------------------------------------------------------------
-- Labels

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

sGraph :: String
sGraph = "subgraph"

sGraph' :: DotCode
sGraph' = text sGraph

clust :: String
clust = "cluster"

clust' :: DotCode
clust' = text clust

-- -----------------------------------------------------------------------------

printGraphID                 :: (a -> Bool) -> (a -> Bool)
                                -> (a -> Maybe GraphID)
                                -> a -> DotCode
printGraphID str isDir mID g = bool empty strGraph' (str g)
                               <+> bool undirGraph' dirGraph' (isDir g)
                               <+> maybe empty toDot (mID g)

parseGraphID   :: (Bool -> Bool -> Maybe GraphID -> a) -> Parse a
parseGraphID f = do allWhitespace'
                    str <- liftM isJust
                           $ optional (parseAndSpace $ string strGraph)
                    dir <- parseAndSpace ( stringRep True dirGraph
                                           `onFail`
                                           stringRep False undirGraph
                                         )
                    gID <- optional $ parseAndSpace parse
                    return $ f str dir gID

printStmtBased          :: (a -> DotCode) -> (a -> b) -> (b -> DotCode)
                           -> a -> DotCode
printStmtBased f r dr a = printBracesBased (f a) (dr $ r a)

printStmtBasedList        :: (a -> DotCode) -> (a -> b) -> (b -> DotCode)
                             -> [a] -> DotCode
printStmtBasedList f r dr = vcat . map (printStmtBased f r dr)

parseStmtBased :: Parse stmt -> Parse (stmt -> a) -> Parse a
parseStmtBased = flip apply . parseBracesBased

-- Can't use the 'braces' combinator here because we want the closing
-- brace lined up with the h value, which due to indentation might not
-- be the case with braces.
printBracesBased     :: DotCode -> DotCode -> DotCode
printBracesBased h i = vcat [ h <+> lbrace
                            , ind i
                            , rbrace
                            ]
  where
    ind = nest 4

parseBracesBased   :: Parse a -> Parse a
parseBracesBased p = whitespace' >> parseBraced (wrapWhitespace p)
                     `adjustErr`
                     (++ "\nNot a valid value wrapped in braces.")


printSubGraphID     :: (a -> (Bool, Maybe GraphID)) -> a -> DotCode
printSubGraphID f a = sGraph'
                      <+> maybe cl dtID mID
  where
    (isCl, mID) = f a
    cl = bool empty clust' isCl
    dtID = printSGID isCl

-- | Print the actual ID for a 'DotSubGraph'.
printSGID          :: Bool -> GraphID -> DotCode
printSGID isCl sID = bool noClust addClust isCl
  where
    noClust = toDot sID
    -- Have to manually render it as we need the un-quoted form.
    addClust = toDot . (++) clust . (:) '_'
               . renderDot $ mkDot sID
    mkDot (Str str) = text str -- Quotes will be escaped later
    mkDot gid       = unqtDot gid

parseSubGraphID   :: (Bool -> Maybe GraphID -> c) -> Parse c
parseSubGraphID f = do string sGraph
                       whitespace
                       liftM (uncurry f) parseSGID

parseSGID :: Parse (Bool, Maybe GraphID)
parseSGID = oneOf [ liftM getClustFrom $ parseAndSpace parse
                  , return (False, Nothing)
                  ]
  where
    -- If it's a String value, check to see if it's actually a
    -- cluster_Blah value; thus need to manually re-parse it.
    getClustFrom (Str str) = runParser' pStr $ Text.pack str
    getClustFrom gid       = (False, Just gid)

    checkCl = stringRep True clust
    pStr = do isCl <- checkCl
                      `onFail`
                      return False
              when isCl $ optional (character '_') >> return ()
              sID <- optional pID
              let sID' = if sID == emptyID
                         then Nothing
                         else sID
              return (isCl, sID')

    emptyID = Just $ Str ""

    -- For Strings, there are no more quotes to unescape, so consume
    -- what you can.
    pID = liftM stringNum (many next)

{- This is a much nicer definition, but unfortunately it doesn't work.
   The problem is that Graphviz decides that a subgraph is a cluster
   if the ID starts with "cluster" (no quotes); thus, we _have_ to do
   the double layer of parsing to get it to work :@

            do isCl <- stringRep True clust
                       `onFail`
                       return False
               sID <- optional $ do when isCl
                                      $ optional (character '_') >> return ()
                                    parseUnqt
               when (isCl || isJust sID) $ whitespace >> return ()
               return (isCl, sID)
-}

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
                      atts <- tryParseList' (whitespace' >> parse)
                      return $ f atts
                   `adjustErr`
                   (++ "\n\nNot a valid attribute-based structure")

parseAttrBasedList :: Parse (Attributes -> a) -> Parse [a]
parseAttrBasedList = parseStatements . parseAttrBased

-- | Parse the separator (and any other whitespace present) between statements.
statementEnd :: Parse ()
statementEnd = parseSplit >> newline'
  where
    parseSplit = (whitespace' >> oneOf [ liftM return $ character ';'
                                       , newline
                                       ]
                 )
                 `onFail`
                 whitespace

parseStatements   :: Parse a -> Parse [a]
parseStatements p = sepBy (whitespace' >> p) statementEnd
                    `discard`
                    optional statementEnd
