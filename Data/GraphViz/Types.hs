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
     Note that if the 'DotSubGraph' has @'isCluster' = 'True'@ then
     this isn't a problem.

   * If you want any 'GlobalAttributes' in a 'DotSubGraph' and want
     them to only apply to that 'DotSubGraph', then you must ensure it
     does indeed have a valid 'GraphID'.

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
     having graph attributes that do not apply to subgraphs\/clusters
     by listing them /after/ the subgraphs\/clusters.

   * The parser will strip out comments and convert multiline strings
     into a single line string.  Pre-processor lines (i.e. those
     started by @#@) and string concatenation are not yet supported.

   See "Data.GraphViz.Attributes" for more limitations.

-}
module Data.GraphViz.Types
    ( -- * The overall representation of a graph in /Dot/ format.
      DotGraph(..)
      -- ** Printing and parsing a @DotGraph@.
    , printDotGraph
    , parseDotGraph
      -- ** Functions acting on a @DotGraph@.
    , setID
    , makeStrict
    , graphNodes
    , graphEdges
      -- ** Reporting of errors in a @DotGraph@.
    , DotError(..)
    , isValidGraph
    , graphErrors
      -- * Sub-components of a @DotGraph@.
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
data DotGraph a = DotGraph { strictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements a
                           }
                  deriving (Eq, Ord, Show, Read)

-- | A strict graph disallows multiple edges.
makeStrict   :: DotGraph a -> DotGraph a
makeStrict g = g { strictGraph = True }

-- | Set the ID of the graph.
setID     :: GraphID -> DotGraph a -> DotGraph a
setID i g = g { graphID = Just i }

-- | Return all the 'DotNode's contained within this 'DotGraph'.  Note
--   that it does not yet return all implicitly defined nodes
--   contained only within 'DotEdge's.
graphNodes :: DotGraph a -> [DotNode a]
graphNodes = statementNodes . graphStatements

-- | Return all the 'DotEdge's contained within this 'DotGraph'.
graphEdges :: DotGraph a -> [DotEdge a]
graphEdges = statementEdges . graphStatements

-- | The actual /Dot/ code for a 'DotGraph'.  Note that it is expected
--   that @'parseDotGraph' . 'printDotGraph' == 'id'@ (this might not
--   be true the other way around due to un-parseable components).
printDotGraph :: (PrintDot a) => DotGraph a -> String
printDotGraph = renderDot . toDot

-- | Parse a limited subset of the Dot language to form a 'DotGraph'
--   (that is, the caveats listed in "Data.GraphViz.Attributes" aside,
--   Dot graphs are parsed if they match the layout of 'DotGraph').
--
--   Also removes any comments, etc. before parsing.
parseDotGraph :: (ParseDot a) => String -> DotGraph a
parseDotGraph = fst . runParser parse . preprocess

-- | Check if all the 'Attribute's are being used correctly.
isValidGraph :: DotGraph a -> Bool
isValidGraph = null . graphErrors

-- | Return detectable errors in the 'DotGraph'.
graphErrors :: DotGraph a -> [DotError a]
graphErrors = invalidStmts usedByGraphs . graphStatements

instance (PrintDot a) => PrintDot (DotGraph a) where
    unqtDot = printStmtBased printGraphID graphStatements

printGraphID   :: (PrintDot a) => DotGraph a -> DotCode
printGraphID g = bool strGraph' empty (strictGraph g)
                 <+> bool dirGraph' undirGraph' (directedGraph g)
                 <+> maybe empty toDot (graphID g)

instance (ParseDot a) => ParseDot (DotGraph a) where
    parseUnqt = parseStmtBased parseGraphID

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid DotGraph")

parseGraphID :: (ParseDot a) => Parse (DotStatements a -> DotGraph a)
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

-- | A polymorphic type that covers all possible ID values allowed by
--   Dot syntax.  Note that whilst the 'ParseDot' and 'PrintDot'
--   instances for 'String' will properly take care of the special
--   cases for numbers, they are treated differently here.
data GraphID = Str String
             | Int Int
             | Dbl Double
             | HTML URL
               deriving (Eq, Ord, Show, Read)

instance PrintDot GraphID where
    unqtDot (Str str) = unqtDot str
    unqtDot (Int i)   = unqtDot i
    unqtDot (Dbl d)   = unqtDot d
    unqtDot (HTML u)  = unqtDot u

    toDot (Str str) = toDot str
    toDot gID       = unqtDot gID

instance ParseDot GraphID where
    parseUnqt = oneOf [ liftM mDbl parseUnqt
                      , liftM Int  parseUnqt
                      , liftM HTML parseUnqt
                        -- Parse last so that quoted numbers are parsed as numbers.
                      , liftM Str  parseUnqt
                      ]

    parse = oneOf [ liftM mDbl parse
                  , liftM Int  parse
                  , liftM HTML parse
                  -- Parse last so that quoted numbers are parsed as numbers.
                  , liftM Str  parse
                  ]
            `adjustErr`
            (++ "Not a valid GraphID")

-- For when an Int gets parsed as a Double
mDbl   :: Double -> GraphID
mDbl d = if fromIntegral i == d
         then Int i
         else Dbl d
  where
    i = round d

-- -----------------------------------------------------------------------------

data DotStatements a = DotStmts { attrStmts :: [GlobalAttributes]
                                , subGraphs :: [DotSubGraph a]
                                , nodeStmts :: [DotNode a]
                                , edgeStmts :: [DotEdge a]
                                }
                     deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotStatements a) where
    unqtDot stmts = vcat [ toDot $ attrStmts stmts
                         , toDot $ subGraphs stmts
                         , toDot $ nodeStmts stmts
                         , toDot $ edgeStmts stmts
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

printStmtBased          :: (PrintDot n) => (a -> DotCode)
                           -> (a -> DotStatements n) -> a -> DotCode
printStmtBased ff fss a = vcat [ ff a <+> lbrace
                               , ind stmts
                               , rbrace
                               ]
    where
      ind = nest 4
      stmts = toDot $ fss a

printStmtBasedList        :: (PrintDot n) => (a -> DotCode)
                             -> (a -> DotStatements n) -> [a] -> DotCode
printStmtBasedList ff fss = vcat . map (printStmtBased ff fss)

parseStmtBased   :: (ParseDot n) => Parse (DotStatements n -> a) -> Parse a
parseStmtBased p = do f <- p
                      whitespace'
                      character '{'
                      newline'
                      stmts <- parse
                      newline'
                      whitespace'
                      character '}'
                      return $ f stmts
                   `adjustErr`
                   (++ "\n\nNot a valid statement-based structure")

parseStmtBasedList   :: (ParseDot n) => Parse (DotStatements n -> a)
                        -> Parse [a]
parseStmtBasedList p = sepBy (whitespace' >> parseStmtBased p) newline'

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
                liftM determineType parse `discard` optional lineEnd

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid listing of global attributes")

    -- Have to do this manually because of the special case
    parseUnqtList = sepBy (whitespace' >> parse) newline'

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
    unqtDot = printStmtBased printSubGraphID subGraphStmts

    unqtListToDot = printStmtBasedList printSubGraphID subGraphStmts

    listToDot = unqtListToDot

printSubGraphID   :: DotSubGraph a -> DotCode
printSubGraphID s = sGraph'
                    <+> maybe cl dtID (subGraphID s)
    where
      isCl = isCluster s
      cl = bool clust' empty isCl
      dtID = printSGID isCl

-- | Print the actual ID for a 'DotSubGraph'.
printSGID          :: Bool -> GraphID -> DotCode
printSGID isCl sID = bool addClust noClust isCl
    where
      noClust = toDot sID
      addClust = toDot . (++) clust . (:) '_'
                 . renderDot $ unqtDot sID

instance (ParseDot a) => ParseDot (DotSubGraph a) where
    parseUnqt = parseStmtBased parseSubGraphID

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid Sub Graph")

    parseUnqtList = parseStmtBasedList parseSubGraphID

    parseList = parseUnqtList

parseSubGraphID :: Parse (DotStatements a -> DotSubGraph a)
parseSubGraphID = do string sGraph
                     whitespace'
                     (isCl,sID) <- parseSGID
                     return $ DotSG isCl sID

parseSGID :: Parse (Bool, Maybe GraphID)
parseSGID = do s <- parse
               return (fst $ runParser pStr s)
            `onFail`
            liftM (flip (,) Nothing) checkCl
  where
    checkCl = liftM isJust $ optional (string clust)
    pStr = do isCl <- checkCl `discard`optional (character '_')
              sID <- parseUnqt
              return (isCl, Just sID)

sGraph :: String
sGraph = "subgraph"

sGraph' :: DotCode
sGraph' = text sGraph

clust :: String
clust = "cluster"

clust' :: DotCode
clust' = text clust

instance Functor DotSubGraph where
    fmap f sg = sg { subGraphStmts = fmap f $ subGraphStmts sg }

invalidSubGraph    :: DotSubGraph a -> [DotError a]
invalidSubGraph sg = invalidStmts valFunc (subGraphStmts sg)
    where
      valFunc = bool usedByClusters usedBySubGraphs (isCluster sg)

subGraphNodes :: DotSubGraph a -> [DotNode a]
subGraphNodes = statementNodes . subGraphStmts

subGraphEdges :: DotSubGraph a -> [DotEdge a]
subGraphEdges = statementEdges . subGraphStmts

-- -----------------------------------------------------------------------------

-- | A node in 'DotGraph' is either a singular node, or a cluster
--   containing nodes (or more clusters) within it.
--   At the moment, clusters are not parsed.
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
parseNodeID = liftM DotNode parse

instance Functor DotNode where
    fmap f n = n { nodeID = f $ nodeID n }

invalidNode   :: DotNode a -> [DotError a]
invalidNode n = map (NodeError (Just $ nodeID n))
                $ filter (not . usedByNodes) (nodeAttributes n)

-- -----------------------------------------------------------------------------

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
printEdgeID e = unqtDot (edgeFromNodeID e)
                <+> bool dirEdge' undirEdge' (directedEdge e)
                <+> unqtDot (edgeToNodeID e)


instance (ParseDot a) => ParseDot (DotEdge a) where
    parseUnqt = parseAttrBased parseEdgeID

    parse = parseUnqt -- Don't want the option of quoting

    -- Have to take into account edges of the type "n1 -> n2 -> n3", etc.
    parseUnqtList = liftM concat
                    $ sepBy (whitespace' >> parseEdgeLine) newline'

    parseList = parseUnqtList

parseEdgeID :: (ParseDot a) => Parse (Attributes -> DotEdge a)
parseEdgeID = do eHead <- parse
                 whitespace'
                 eType <- parseEdgeType
                 whitespace'
                 eTail <- parse
                 return $ DotEdge eHead eTail eType

parseEdgeType :: Parse Bool
parseEdgeType = stringRep True dirEdge
                `onFail`
                stringRep False undirEdge

parseEdgeLine :: (ParseDot a) => Parse [DotEdge a]
parseEdgeLine = liftM return parse
                `onFail`
                do n1 <- parse
                   ens <- many1 $ do whitespace'
                                     eType <- parseEdgeType
                                     whitespace'
                                     n <- parse
                                     return (eType, n)
                   let ens' = (True, n1) : ens
                       efs = zipWith mkEdg ens' (tail ens')
                       ef = return $ \ as -> map ($as) efs
                   parseAttrBased ef
    where
      mkEdg (_, hn) (et, tn) = DotEdge hn tn et

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
                      atts <- tryParseList
                      lineEnd
                      return $ f atts
                   `adjustErr`
                   (++ "\n\nNot a valid attribute-based structure")

parseAttrBasedList   :: Parse (Attributes -> a) -> Parse [a]
parseAttrBasedList p = sepBy (whitespace' >> parseAttrBased p) newline'

lineEnd :: Parse ()
lineEnd = whitespace' >> character ';' >> return ()
