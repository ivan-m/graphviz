{-# LANGUAGE CPP, OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Types.Internal.Common
   Description : Common internal functions for dealing with overall types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides common functions used by both
   "Data.GraphViz.Types" as well as "Data.GraphViz.Types.Generalised".
-}
module Data.GraphViz.Types.Internal.Common
       ( GraphID (..)
       , Number (..)
       , numericValue
       , GlobalAttributes (..)
       , partitionGlobal
       , unPartitionGlobal
       , withGlob
       , DotNode (..)
       , DotEdge (..)
       , parseEdgeLine
       , printGraphID
       , parseGraphID
       , printStmtBased
       , printStmtBasedList
       , printSubGraphID
       , parseSubGraph
       , parseBracesBased
       , parseStatements
       ) where

import Data.GraphViz.Attributes.Complete (Attribute(HeadPort, TailPort),
                                          Attributes, Number(..),
                                          usedByClusters, usedByGraphs,
                                          usedByNodes)
import Data.GraphViz.Attributes.Internal (PortPos, parseEdgeBasedPP)
import Data.GraphViz.Internal.State
import Data.GraphViz.Internal.Util
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import           Control.Monad       (unless, when)
import           Data.Maybe          (isJust)
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as T
import qualified Data.Text.Lazy.Read as T

#if !MIN_VERSION_base (4,13,0)
import Data.Monoid ((<>))
#endif

-- -----------------------------------------------------------------------------
-- This is re-exported by Data.GraphViz.Types

-- | A polymorphic type that covers all possible ID values allowed by
--   Dot syntax.  Note that whilst the 'ParseDot' and 'PrintDot'
--   instances for 'String' will properly take care of the special
--   cases for numbers, they are treated differently here.
data GraphID = Str Text
             | Num Number
             deriving (Eq, Ord, Show, Read)

instance PrintDot GraphID where
  unqtDot (Str str) = unqtDot str
  unqtDot (Num n)   = unqtDot n

  toDot (Str str) = toDot str
  toDot (Num n)   = toDot n

instance ParseDot GraphID where
  parseUnqt = stringNum <$> parseUnqt

  parse = stringNum <$> parse
          `adjustErr`
          ("Not a valid GraphID\n\t"++)

stringNum     :: Text -> GraphID
stringNum str = maybe checkDbl (Num . Int) $ stringToInt str
  where
    checkDbl = if isNumString True str
               then Num . Dbl $ toDouble str
               else Str str

numericValue           :: GraphID -> Maybe Int
numericValue (Str str) = either (const Nothing) (Just . round . fst)
                         $ T.signed T.double str
numericValue (Num n)   = case n of
                           Int i -> Just i
                           Dbl d -> Just $ round d

-- -----------------------------------------------------------------------------

-- Re-exported by Data.GraphViz.Types.*

-- | Represents a list of top-level list of 'Attribute's for the
--   entire graph/sub-graph.  Note that 'GraphAttrs' also applies to
--   'DotSubGraph's.
--
--   Note that Dot allows a single 'Attribute' to be listed on a line;
--   if this is the case then when parsing, the type of 'Attribute' it
--   is determined and that type of 'GlobalAttribute' is created.
data GlobalAttributes = GraphAttrs { attrs :: Attributes }
                      | NodeAttrs  { attrs :: Attributes }
                      | EdgeAttrs  { attrs :: Attributes }
                      deriving (Eq, Ord, Show, Read)

instance PrintDot GlobalAttributes where
  unqtDot = printAttrBased True printGlobAttrType globAttrType attrs

  unqtListToDot = printAttrBasedList True printGlobAttrType globAttrType attrs

  listToDot = unqtListToDot

-- GraphAttrs, NodeAttrs and EdgeAttrs respectively
partitionGlobal :: [GlobalAttributes] -> (Attributes, Attributes, Attributes)
partitionGlobal = foldr select ([], [], [])
  where
    select globA ~(gs,ns,es) = case globA of
                                 GraphAttrs as -> (as ++ gs, ns, es)
                                 NodeAttrs  as -> (gs, as ++ ns, es)
                                 EdgeAttrs  as -> (gs, ns, as ++ es)

unPartitionGlobal :: (Attributes, Attributes, Attributes) -> [GlobalAttributes]
unPartitionGlobal (gas,nas,eas) = [ GraphAttrs gas
                                  , NodeAttrs  nas
                                  , EdgeAttrs  eas
                                  ]

printGlobAttrType              :: GlobalAttributes -> DotCode
printGlobAttrType GraphAttrs{} = text "graph"
printGlobAttrType NodeAttrs{}  = text "node"
printGlobAttrType EdgeAttrs{}  = text "edge"

instance ParseDot GlobalAttributes where
  -- Not using parseAttrBased here because we want to force usage of
  -- Attributes.
  parseUnqt = do gat <- parseGlobAttrType

                 -- Determine if we need to set the attribute type.
                 let mtp = globAttrType $ gat [] -- Only need the constructor
                 oldTp <- getAttributeType
                 maybe (return ()) setAttributeType mtp

                 as <- whitespace *> parse

                 -- Safe to set back even if not changed.
                 setAttributeType oldTp
                 return $ gat as
              `onFail`
              fmap determineType parse

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          ("Not a valid listing of global attributes\n\t"++)

  -- Have to do this manually because of the special case
  parseUnqtList = parseStatements parseUnqt

  parseList = parseUnqtList

-- Cheat: rather than determine whether it's a graph, cluster or
-- sub-graph just don't set it.
globAttrType :: GlobalAttributes -> Maybe AttributeType
globAttrType NodeAttrs{} = Just NodeAttribute
globAttrType EdgeAttrs{} = Just EdgeAttribute
globAttrType _           = Nothing

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

withGlob :: (Attributes -> Attributes) -> GlobalAttributes -> GlobalAttributes
withGlob f (GraphAttrs as) = GraphAttrs $ f as
withGlob f (NodeAttrs  as) = NodeAttrs  $ f as
withGlob f (EdgeAttrs  as) = EdgeAttrs  $ f as

-- -----------------------------------------------------------------------------

-- | A node in 'DotGraph'.
data DotNode n = DotNode { nodeID         :: n
                         , nodeAttributes :: Attributes
                         }
               deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotNode n) where
  unqtDot = printAttrBased False printNodeID
                           (const $ Just NodeAttribute) nodeAttributes

  unqtListToDot = printAttrBasedList False printNodeID
                                     (const $ Just NodeAttribute) nodeAttributes

  listToDot = unqtListToDot

printNodeID :: (PrintDot n) => DotNode n -> DotCode
printNodeID = toDot . nodeID

instance (ParseDot n) => ParseDot (DotNode n) where
  parseUnqt = parseAttrBased NodeAttribute False parseNodeID

  parse = parseUnqt -- Don't want the option of quoting

  parseUnqtList = parseAttrBasedList NodeAttribute False parseNodeID

  parseList = parseUnqtList

parseNodeID :: (ParseDot n) => Parse (Attributes -> DotNode n)
parseNodeID = DotNode <$> parseAndCheck
  where
    parseAndCheck = do n <- parse
                       me <- optional parseUnwanted
                       maybe (return n) (const notANode) me
    notANode = fail "This appears to be an edge, not a node"
    parseUnwanted = oneOf [ parseEdgeType *> return ()
                          , character ':' *> return () -- PortPos value
                          ]

instance Functor DotNode where
  fmap f n = n { nodeID = f $ nodeID n }

-- -----------------------------------------------------------------------------

-- This is re-exported in Data.GraphViz.Types; defined here so that
-- Generalised can access and use parseEdgeLine (needed for "a -> b ->
-- c"-style edge statements).

-- | An edge in 'DotGraph'.
data DotEdge n = DotEdge { fromNode       :: n
                         , toNode         :: n
                         , edgeAttributes :: Attributes
                         }
               deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotEdge n) where
  unqtDot = printAttrBased False printEdgeID
                           (const $ Just EdgeAttribute) edgeAttributes

  unqtListToDot = printAttrBasedList False printEdgeID
                                     (const $ Just EdgeAttribute) edgeAttributes

  listToDot = unqtListToDot

printEdgeID   :: (PrintDot n) => DotEdge n -> DotCode
printEdgeID e = do isDir <- getDirectedness
                   toDot (fromNode e)
                     <+> bool undirEdge' dirEdge' isDir
                     <+> toDot (toNode e)


instance (ParseDot n) => ParseDot (DotEdge n) where
  parseUnqt = parseAttrBased EdgeAttribute False parseEdgeID

  parse = parseUnqt -- Don't want the option of quoting

  -- Have to take into account edges of the type "n1 -> n2 -> n3", etc.
  parseUnqtList = concat <$> parseStatements parseEdgeLine

  parseList = parseUnqtList

parseEdgeID :: (ParseDot n) => Parse (Attributes -> DotEdge n)
parseEdgeID = ignoreSep mkEdge parseEdgeNode parseEdgeType parseEdgeNode
              `adjustErr`
              ("Parsed beginning of DotEdge but could not parse Attributes:\n\t"++)
              -- Parse both edge types just to be more liberal

type EdgeNode n = (n, Maybe PortPos)

-- | Takes into account edge statements containing something like
--   @a -> \{b c\}@.
parseEdgeNodes :: (ParseDot n) => Parse [EdgeNode n]
parseEdgeNodes = oneOf [ parseBraced (wrapWhitespace
                                      -- Should really use sepBy1, but this will do.
                                      $ parseStatements parseEdgeNode)
                       , sepBy1 parseEdgeNode (wrapWhitespace parseComma)
                       , (: []) <$> parseEdgeNode
                       ]

parseEdgeNode :: (ParseDot n) => Parse (EdgeNode n)
parseEdgeNode = liftA2 (,) parse
                           (optional $ character ':' *> parseEdgeBasedPP)

mkEdge :: EdgeNode n -> EdgeNode n -> Attributes -> DotEdge n
mkEdge (eFrom, mFP) (eTo, mTP) = DotEdge eFrom eTo
                                 . addPortPos TailPort mFP
                                 . addPortPos HeadPort mTP

mkEdges :: [EdgeNode n] -> [EdgeNode n]
           -> Attributes -> [DotEdge n]
mkEdges fs ts as = liftA2 (\f t -> mkEdge f t as) fs ts

addPortPos   :: (PortPos -> Attribute) -> Maybe PortPos
                -> Attributes -> Attributes
addPortPos c = maybe id ((:) . c)

parseEdgeType :: Parse Bool
parseEdgeType = wrapWhitespace $ stringRep True dirEdge
                                 `onFail`
                                 stringRep False undirEdge

parseEdgeLine :: (ParseDot n) => Parse [DotEdge n]
parseEdgeLine = do n1 <- parseEdgeNodes
                   ens <- many1 $ parseEdgeType *> parseEdgeNodes
                   let ens' = n1 : ens
                       efs = zipWith mkEdges ens' (tail ens')
                       ef = return $ \ as -> concatMap ($as) efs
                   parseAttrBased EdgeAttribute False ef

instance Functor DotEdge where
  fmap f e = e { fromNode = f $ fromNode e
               , toNode   = f $ toNode e
               }

dirEdge :: String
dirEdge = "->"

dirEdge' :: DotCode
dirEdge' = text $ T.pack dirEdge

undirEdge :: String
undirEdge = "--"

undirEdge' :: DotCode
undirEdge' = text $ T.pack undirEdge

-- -----------------------------------------------------------------------------
-- Labels

dirGraph :: String
dirGraph = "digraph"

dirGraph' :: DotCode
dirGraph' = text $ T.pack dirGraph

undirGraph :: String
undirGraph = "graph"

undirGraph' :: DotCode
undirGraph' = text $ T.pack undirGraph

strGraph :: String
strGraph = "strict"

strGraph' :: DotCode
strGraph' = text $ T.pack strGraph

sGraph :: String
sGraph = "subgraph"

sGraph' :: DotCode
sGraph' = text $ T.pack sGraph

clust :: String
clust = "cluster"

clust' :: DotCode
clust' = text $ T.pack clust

-- -----------------------------------------------------------------------------

printGraphID                 :: (a -> Bool) -> (a -> Bool)
                                -> (a -> Maybe GraphID)
                                -> a -> DotCode
printGraphID str isDir mID g = do setDirectedness isDir'
                                  bool empty strGraph' (str g)
                                    <+> bool undirGraph' dirGraph' isDir'
                                    <+> maybe empty toDot (mID g)
  where
    isDir' = isDir g

parseGraphID   :: (Bool -> Bool -> Maybe GraphID -> a) -> Parse a
parseGraphID f = do whitespace
                    str <- isJust <$> optional (parseAndSpace $ string strGraph)
                    dir <- parseAndSpace ( stringRep True dirGraph
                                           `onFail`
                                           stringRep False undirGraph
                                         )
                    setDirectedness dir
                    gID <- optional $ parseAndSpace parse
                    return $ f str dir gID

printStmtBased              :: (a -> DotCode) -> (a -> AttributeType)
                               -> (a -> stmts) -> (stmts -> DotCode)
                               -> a -> DotCode
printStmtBased f ftp r dr a = do gs <- getsGS id
                                 setAttributeType $ ftp a
                                 dc <- printBracesBased (f a) (dr $ r a)
                                 modifyGS (const gs)
                                 return dc

printStmtBasedList            :: (a -> DotCode) -> (a -> AttributeType)
                                 -> (a -> stmts) -> (stmts -> DotCode)
                                 -> [a] -> DotCode
printStmtBasedList f ftp r dr = vcat . mapM (printStmtBased f ftp r dr)

-- Can't use the 'braces' combinator here because we want the closing
-- brace lined up with the h value, which due to indentation might not
-- be the case with braces.
printBracesBased     :: DotCode -> DotCode -> DotCode
printBracesBased h i = vcat $ sequence [ h <+> lbrace
                                       , ind i
                                       , rbrace
                                       ]
  where
    ind = indent 4

-- | This /must/ only be used for sub-graphs, etc.
parseBracesBased      :: AttributeType -> Parse a -> Parse a
parseBracesBased tp p = do gs <- getsGS id
                           setAttributeType tp
                           a <- whitespace *> parseBraced (wrapWhitespace p)
                           modifyGS (const gs)
                           return a
                        `adjustErr`
                        ("Not a valid value wrapped in braces.\n\t"++)

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
    addClust = toDot . T.append (T.pack clust) . T.cons '_'
               . renderDot $ mkDot sID
    mkDot (Str str) = text str -- Quotes will be escaped later
    mkDot gid       = unqtDot gid

parseSubGraph         :: (Bool -> Maybe GraphID -> stmt -> c) -> Parse stmt -> Parse c
parseSubGraph pid pst = do (isC, fID) <- parseSubGraphID pid
                           let tp = bool SubGraphAttribute ClusterAttribute isC
                           fID <$> parseBracesBased tp pst

parseSubGraphID   :: (Bool -> Maybe GraphID -> c) -> Parse (Bool,c)
parseSubGraphID f = appl <$> (string sGraph *> whitespace1 *> parseSGID)
  where
    appl (isC, mid) = (isC, f isC mid)

parseSGID :: Parse (Bool, Maybe GraphID)
parseSGID = oneOf [ getClustFrom <$> parseAndSpace parse
                  , return (False, Nothing)
                  ]
  where
    -- If it's a String value, check to see if it's actually a
    -- cluster_Blah value; thus need to manually re-parse it.
    getClustFrom (Str str) = runParser' pStr str
    getClustFrom gid       = (False, Just gid)

    checkCl = stringRep True clust
    pStr = do isCl <- checkCl
                      `onFail`
                      return False
              when isCl $ optional (character '_') *> return ()
              sID <- optional pID
              let sID' = if sID == emptyID
                         then Nothing
                         else sID
              return (isCl, sID')

    emptyID = Just $ Str ""

    -- For Strings, there are no more quotes to unescape, so consume
    -- what you can.
    pID = stringNum <$> manySatisfy (const True)

{- This is a much nicer definition, but unfortunately it doesn't work.
   The problem is that Graphviz decides that a subgraph is a cluster
   if the ID starts with "cluster" (no quotes); thus, we _have_ to do
   the double layer of parsing to get it to work :@

            do isCl <- stringRep True clust
                       `onFail`
                       return False
               sID <- optional $ do when isCl
                                      $ optional (character '_') *> return ()
                                    parseUnqt
               when (isCl || isJust sID) $ whitespace1 *> return ()
               return (isCl, sID)
-}

-- The Bool is True for global, False for local.
printAttrBased                    :: Bool -> (a -> DotCode) -> (a -> Maybe AttributeType)
                                     -> (a -> Attributes) -> a -> DotCode
printAttrBased prEmp ff ftp fas a = do oldType <- getAttributeType
                                       maybe (return ()) setAttributeType mtp
                                       oldCS <- getColorScheme
                                       (dc <> semi) <* unless prEmp (setColorScheme oldCS)
                                                    <* setAttributeType oldType
  where
    mtp = ftp a
    f = ff a
    dc = case fas a of
           [] | not prEmp -> f
           as -> f <+> toDot as

-- The Bool is True for global, False for local.
printAttrBasedList                    :: Bool -> (a -> DotCode) -> (a -> Maybe AttributeType)
                                         -> (a -> Attributes) -> [a] -> DotCode
printAttrBasedList prEmp ff ftp fas = vcat . mapM (printAttrBased prEmp ff ftp fas)

-- The Bool is True for global, False for local.
parseAttrBased         :: AttributeType -> Bool -> Parse (Attributes -> a) -> Parse a
parseAttrBased tp lc p = do oldType <- getAttributeType
                            setAttributeType tp
                            oldCS <- getColorScheme
                            f <- p
                            atts <- tryParseList' (whitespace *> parse)
                            unless lc $ setColorScheme oldCS
                            when (tp /= oldType) $ setAttributeType oldType
                            return $ f atts
                         `adjustErr`
                         ("Not a valid attribute-based structure\n\t"++)

-- The Bool is True for global, False for local.
parseAttrBasedList       :: AttributeType -> Bool -> Parse (Attributes -> a) -> Parse [a]
parseAttrBasedList tp lc = parseStatements . parseAttrBased tp lc

-- | Parse the separator (and any other whitespace1 present) between statements.
statementEnd :: Parse ()
statementEnd = parseSplit *> newline'
  where
    parseSplit = (whitespace *> oneOf [ character ';' *> return ()
                                      , newline
                                      ]
                 )
                 `onFail`
                 whitespace1

parseStatements   :: Parse a -> Parse [a]
parseStatements p = sepBy (whitespace *> p) statementEnd
                    `discard`
                    optional statementEnd
