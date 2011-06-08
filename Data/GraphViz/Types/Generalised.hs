{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
   Module      : Data.GraphViz.Types.Generalised.
   Description : Alternate definition of the Graphviz types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides an alternate definition of the types found in
   "Data.GraphViz.Types.Canonical", in that there is no limitation on the
   ordering of statements.

   The types here have the same names as those in
   "Data.GraphViz.Types.Canonical", and as such if both are being used
   then at least one must be imported qualified.

 -}
module Data.GraphViz.Types.Generalised
       ( DotGraph(..)
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

import Data.GraphViz.Types
import qualified Data.GraphViz.Types.Canonical as C
import Data.GraphViz.Types.Common
import Data.GraphViz.Types.State
import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Util(bool)

import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (><))
import qualified Data.Foldable as F
import Control.Arrow((&&&))
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | The internal representation of a generalised graph in Dot form.
data DotGraph a = DotGraph { -- | If 'True', no multiple edges are drawn.
                             strictGraph     :: Bool
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements a
                           }
                deriving (Eq, Ord, Show, Read)

instance (Ord n, PrintDot n, ParseDot n) => DotRepr DotGraph n where
  fromCanonical = generaliseDotGraph

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

instance (PrintDot a) => PrintDot (DotGraph a) where
  unqtDot = printStmtBased printGraphID' graphStatements printGStmts
    where
      printGraphID' = printGraphID strictGraph directedGraph graphID

instance (ParseDot a) => ParseDot (DotGraph a) where
  parseUnqt = parseStmtBased parseGStmts (parseGraphID DotGraph)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "\n\nNot a valid DotGraph")

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

type DotStatements a = Seq (DotStatement a)

printGStmts :: (PrintDot a) => DotStatements a -> DotCode
printGStmts = toDot . F.toList

parseGStmts :: (ParseDot a) => Parse (DotStatements a)
parseGStmts = liftM Seq.fromList parse

statementStructure :: DotStatements a -> GraphState ()
statementStructure = F.mapM_ stmtStructure

statementNodes :: (Ord a) => DotStatements a -> NodeState a ()
statementNodes = F.mapM_ stmtNodes

statementEdges :: DotStatements a -> EdgeState a ()
statementEdges = F.mapM_ stmtEdges

generaliseStatements       :: C.DotStatements a -> DotStatements a
generaliseStatements stmts = atts >< sgs >< ns >< es
  where
    atts = Seq.fromList . map GA $ C.attrStmts stmts
    sgs  = Seq.fromList . map (SG . generaliseSubGraph) $ C.subGraphs stmts
    ns   = Seq.fromList . map DN $ C.nodeStmts stmts
    es   = Seq.fromList . map DE $ C.edgeStmts stmts


data DotStatement a = GA GlobalAttributes
                    | SG (DotSubGraph a)
                    | DN (DotNode a)
                    | DE (DotEdge a)
                    deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotStatement a) where
  unqtDot (GA ga) = unqtDot ga
  unqtDot (SG sg) = unqtDot sg
  unqtDot (DN dn) = unqtDot dn
  unqtDot (DE de) = unqtDot de

  unqtListToDot = vcat . mapM unqtDot

  listToDot = unqtListToDot

instance (ParseDot a) => ParseDot (DotStatement a) where
  parseUnqt = oneOf [ liftM GA parseUnqt
                    , liftM SG parseUnqt
                    , liftM DN parseUnqt
                    , liftM DE parseUnqt
                    ]

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "Not a valid statement")

  parseUnqtList = liftM concat . wrapWhitespace
                  $ parseStatements p
    where
      -- Have to do something special here because of "a -> b -> c"
      -- syntax for edges.
      p = liftM (map DE) parseEdgeLine
          `onFail`
          liftM return parse

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

stmtNodes         :: (Ord a) => DotStatement a -> NodeState a ()
stmtNodes (GA ga) = addNodeGlobals ga
stmtNodes (SG sg) = withSubGraphID recursiveCall statementNodes sg
stmtNodes (DN dn) = addNode dn
stmtNodes (DE de) = addEdgeNodes de

stmtEdges         :: DotStatement a -> EdgeState a ()
stmtEdges (GA ga) = addEdgeGlobals ga
stmtEdges (SG sg) = withSubGraphID recursiveCall statementEdges sg
stmtEdges (DE de) = addEdge de
stmtEdges _       = return ()

-- -----------------------------------------------------------------------------

data DotSubGraph a = DotSG { isCluster     :: Bool
                             , subGraphID    :: Maybe GraphID
                             , subGraphStmts :: DotStatements a
                             }
                    deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (DotSubGraph a) where
  unqtDot = printStmtBased printSubGraphID' subGraphStmts printGStmts

  unqtListToDot = printStmtBasedList printSubGraphID' subGraphStmts printGStmts

  listToDot = unqtListToDot

printSubGraphID' :: DotSubGraph a -> DotCode
printSubGraphID' = printSubGraphID (isCluster &&& subGraphID)

instance (ParseDot a) => ParseDot (DotSubGraph a) where
  parseUnqt = parseStmtBased parseGStmts (parseSubGraphID DotSG)
              `onFail`
              -- Take anonymous DotSubGraphs into account
              liftM (DotSG False Nothing) (parseBracesBased parseGStmts)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "\n\nNot a valid Sub Graph")

  parseUnqtList = sepBy (whitespace' >> parseUnqt) newline'

  parseList = parseUnqtList

instance Functor DotSubGraph where
  fmap f sg = sg { subGraphStmts = (fmap . fmap) f $ subGraphStmts sg }

generaliseSubGraph :: C.DotSubGraph a -> DotSubGraph a
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
