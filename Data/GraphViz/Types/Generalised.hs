{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{- |
   Module      : Data.GraphViz.Types.Generalised.
   Description : Alternate definition of the Graphviz types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   The generalised Dot representation most closely matches the
   implementation of actual Dot code, as it places no restrictions on
   ordering of elements, etc.  As such it should be able to parse any
   existing Dot code (taking into account the parsing
   limitations/assumptions).

   The sample graph could be implemented (this is actually a prettied
   version of parsing in the Dot code) as:

   > DotGraph { strictGraph = False
   >          , directedGraph = True
   >          , graphID = Just (Str "G")
   >          , graphStatements = Seq.fromList [ SG $ DotSG { isCluster = True
   >                                                        , subGraphID = Just (Int 0)
   >                                                        , subGraphStmts = Seq.fromList [ GA $ GraphAttrs [style filled]
   >                                                                                       , GA $ GraphAttrs [color LightGray]
   >                                                                                       , GA $ NodeAttrs [style filled, color White]
   >                                                                                       , DE $ DotEdge "a0" "a1" []
   >                                                                                       , DE $ DotEdge "a1" "a2" []
   >                                                                                       , DE $ DotEdge "a2" "a3" []
   >                                                                                       , GA $ GraphAttrs [textLabel "process #1"]]}
   >                                           , SG $ DotSG { isCluster = True
   >                                                        , subGraphID = Just (Int 1)
   >                                                        , subGraphStmts = fromList [ GA $ NodeAttrs [style filled]
   >                                                                                   , DE $ DotEdge "b0" "b1" []
   >                                                                                   , DE $ DotEdge "b1" "b2" []
   >                                                                                   , DE $ DotEdge "b2" "b3" []
   >                                                                                   , GA $ GraphAttrs [textLabel "process #2"]
   >                                                                                   , GA $ GraphAttrs [color Blue]]}
   >                                           , DE $ DotEdge "start" "a0" []
   >                                           , DE $ DotEdge "start" "b0" []
   >                                           , DE $ DotEdge "a1" "b3" []
   >                                           , DE $ DotEdge "b2" "a3" []
   >                                           , DE $ DotEdge "a3" "a0" []
   >                                           , DE $ DotEdge "a3" "end" []
   >                                           , DE $ DotEdge "b3" "end" []
   >                                           , DN $ DotNode "start" [shape MDiamond]
   >                                           , DN $ DotNode "end" [shape MSquare]]}

 -}
module Data.GraphViz.Types.Generalised
       ( DotGraph(..)
       , FromGeneralisedDot (..)
         -- * Sub-components of a @DotGraph@.
       , DotStatements
       , DotStatement(..)
       , DotSubGraph(..)
         -- * Re-exported from @Data.GraphViz.Types@.
       , GraphID(..)
       , GlobalAttributes(..)
       , DotNode(..)
       , DotEdge(..)
       ) where

import           Data.GraphViz.Algorithms            (canonicalise)
import           Data.GraphViz.Internal.State        (AttributeType(..))
import           Data.GraphViz.Internal.Util         (bool)
import           Data.GraphViz.Parsing
import           Data.GraphViz.Printing
import           Data.GraphViz.Types
import qualified Data.GraphViz.Types.Canonical       as C
import           Data.GraphViz.Types.Internal.Common
import           Data.GraphViz.Types.State

import           Control.Arrow       ((&&&))
import           Control.Monad.State (evalState, execState, get, modify, put)
import qualified Data.Foldable       as F
import           Data.Sequence       (Seq, (><))
import qualified Data.Sequence       as Seq
import qualified Data.Traversable    as T

-- -----------------------------------------------------------------------------

-- | The internal representation of a generalised graph in Dot form.
data DotGraph n = DotGraph { -- | If 'True', no multiple edges are drawn.
                             strictGraph     :: Bool
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements n
                           }
                deriving (Eq, Ord, Show, Read)

instance (Ord n) => DotRepr DotGraph n where
  fromCanonical = generaliseDotGraph

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

instance (PrintDot n) => PrintDot (DotGraph n) where
  unqtDot = printStmtBased printGraphID' (const GraphAttribute)
                           graphStatements printGStmts
    where
      printGraphID' = printGraphID strictGraph directedGraph graphID

instance (ParseDot n) => ParseDot (DotGraph n) where
  parseUnqt = parseGraphID DotGraph
              <*> parseBracesBased GraphAttribute parseGStmts

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          ("Not a valid generalised DotGraph\n\t"++)

-- | Assumed to be an injective mapping function.
instance Functor DotGraph where
  fmap f g = g { graphStatements = (fmap . fmap) f $ graphStatements g }

-- | Convert a 'DotGraph' to a 'DotGraph', keeping the same order of
--   statements.
generaliseDotGraph    :: C.DotGraph n -> DotGraph n
generaliseDotGraph dg = DotGraph { strictGraph     = C.strictGraph dg
                                 , directedGraph   = C.directedGraph dg
                                 , graphID         = C.graphID dg
                                 , graphStatements = generaliseStatements
                                                     $ C.graphStatements dg
                                 }

-- -----------------------------------------------------------------------------

-- | This class is useful for being able to parse in a dot graph as a
--   generalised one, and then convert it to your preferred
--   representation.
--
--   This can be seen as a semi-inverse of 'fromCanonical'.
class (DotRepr dg n) => FromGeneralisedDot dg n where
  fromGeneralised :: DotGraph n -> dg n

instance (Ord n) => FromGeneralisedDot C.DotGraph n where
  fromGeneralised = canonicalise

instance (Ord n) => FromGeneralisedDot DotGraph n where
  fromGeneralised = id

-- -----------------------------------------------------------------------------

type DotStatements n = Seq (DotStatement n)

printGStmts :: (PrintDot n) => DotStatements n -> DotCode
printGStmts = toDot . F.toList

parseGStmts :: (ParseDot n) => Parse (DotStatements n)
parseGStmts = (Seq.fromList <$> parse)
              `adjustErr`
              ("Not a valid generalised DotStatements\n\t"++)

statementStructure :: DotStatements n -> GraphState ()
statementStructure = F.mapM_ stmtStructure

statementNodes :: (Ord n) => DotStatements n -> NodeState n ()
statementNodes = F.mapM_ stmtNodes

statementEdges :: DotStatements n -> EdgeState n ()
statementEdges = F.mapM_ stmtEdges

generaliseStatements       :: C.DotStatements n -> DotStatements n
generaliseStatements stmts = atts >< sgs >< ns >< es
  where
    atts = Seq.fromList . map GA $ C.attrStmts stmts
    sgs  = Seq.fromList . map (SG . generaliseSubGraph) $ C.subGraphs stmts
    ns   = Seq.fromList . map DN $ C.nodeStmts stmts
    es   = Seq.fromList . map DE $ C.edgeStmts stmts


data DotStatement n = GA GlobalAttributes
                    | SG (DotSubGraph n)
                    | DN (DotNode n)
                    | DE (DotEdge n)
                    deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotStatement n) where
  unqtDot (GA ga) = unqtDot ga
  unqtDot (SG sg) = unqtDot sg
  unqtDot (DN dn) = unqtDot dn
  unqtDot (DE de) = unqtDot de

  unqtListToDot = vcat . mapM unqtDot

  listToDot = unqtListToDot

instance (ParseDot n) => ParseDot (DotStatement n) where
  parseUnqt = oneOf [ GA <$> parseUnqt
                    , SG <$> parseUnqt
                    , DN <$> parseUnqt
                    , DE <$> parseUnqt
                    ]

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          ("Not a valid statement\n\t"++)

  parseUnqtList = fmap concat . wrapWhitespace
                  $ parseStatements p
    where
      -- Have to do something special here because of "a -> b -> c"
      -- syntax for edges.
      p = fmap (map DE) parseEdgeLine
          `onFail`
          fmap (:[]) parse

  parseList = parseUnqtList

instance Functor DotStatement where
  fmap _ (GA ga) = GA ga -- Have to re-make this to make the type checker happy.
  fmap f (SG sg) = SG $ fmap f sg
  fmap f (DN dn) = DN $ fmap f dn
  fmap f (DE de) = DE $ fmap f de

stmtStructure         :: DotStatement n -> GraphState ()
stmtStructure (GA ga) = addGraphGlobals ga
stmtStructure (SG sg) = withSubGraphID addSubGraph statementStructure sg
stmtStructure _       = return ()

stmtNodes         :: (Ord n) => DotStatement n -> NodeState n ()
stmtNodes (GA ga) = addNodeGlobals ga
stmtNodes (SG sg) = withSubGraphID recursiveCall statementNodes sg
stmtNodes (DN dn) = addNode dn
stmtNodes (DE de) = addEdgeNodes de

stmtEdges         :: DotStatement n -> EdgeState n ()
stmtEdges (GA ga) = addEdgeGlobals ga
stmtEdges (SG sg) = withSubGraphID recursiveCall statementEdges sg
stmtEdges (DE de) = addEdge de
stmtEdges _       = return ()

-- -----------------------------------------------------------------------------

data DotSubGraph n = DotSG { isCluster     :: Bool
                           , subGraphID    :: Maybe GraphID
                           , subGraphStmts :: DotStatements n
                           }
                   deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotSubGraph n) where
  unqtDot = printStmtBased printSubGraphID' subGraphAttrType
                           subGraphStmts printGStmts

  unqtListToDot = printStmtBasedList printSubGraphID' subGraphAttrType
                                     subGraphStmts printGStmts

  listToDot = unqtListToDot

subGraphAttrType :: DotSubGraph n -> AttributeType
subGraphAttrType = bool SubGraphAttribute ClusterAttribute . isCluster

printSubGraphID' :: DotSubGraph n -> DotCode
printSubGraphID' = printSubGraphID (isCluster &&& subGraphID)

instance (ParseDot n) => ParseDot (DotSubGraph n) where
  parseUnqt = parseSubGraph DotSG parseGStmts
              `onFail`
              -- Take anonymous DotSubGraphs into account
              fmap (DotSG False Nothing)
                   (parseBracesBased SubGraphAttribute parseGStmts)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          ("Not a valid Sub Graph\n\t"++)

  parseUnqtList = sepBy (whitespace *> parseUnqt) newline'

  parseList = parseUnqtList

instance Functor DotSubGraph where
  fmap f sg = sg { subGraphStmts = (fmap . fmap) f $ subGraphStmts sg }

generaliseSubGraph :: C.DotSubGraph n -> DotSubGraph n
generaliseSubGraph (C.DotSG isC mID stmts) = DotSG { isCluster     = isC
                                                   , subGraphID    = mID
                                                   , subGraphStmts = stmts'
                                                   }
  where
    stmts' = generaliseStatements stmts

withSubGraphID        :: (Maybe (Maybe GraphID) -> b -> a)
                         -> (DotStatements n -> b) -> DotSubGraph n -> a
withSubGraphID f g sg = f mid . g $ subGraphStmts sg
  where
    mid = bool Nothing (Just $ subGraphID sg) $ isCluster sg

renumber    :: DotGraph n -> DotGraph n
renumber dg = dg { graphStatements = newStmts }
  where
    startN = succ $ maxSGInt dg

    newStmts = evalState (stsRe $ graphStatements dg) startN

    stsRe = T.mapM stRe
    stRe (SG sg) = SG <$> sgRe sg
    stRe stmt    = pure stmt
    sgRe sg = do sgid' <- case subGraphID sg of
                            Nothing -> do n <- get
                                          put $ succ n
                                          return . Just . Num $ Int n
                            sgid    -> return sgid
                 stmts' <- stsRe $ subGraphStmts sg
                 return $ sg { subGraphID    = sgid'
                             , subGraphStmts = stmts'
                             }

maxSGInt    :: DotGraph n -> Int
maxSGInt dg = execState (stsInt $ graphStatements dg)
              . (`check` 0)
              $ graphID dg
  where
    check = maybe id max . (numericValue =<<)

    stsInt = F.mapM_ stInt
    stInt (SG sg) = sgInt sg
    stInt _       = return ()
    sgInt sg = do modify (check $ subGraphID sg)
                  stsInt $ subGraphStmts sg
