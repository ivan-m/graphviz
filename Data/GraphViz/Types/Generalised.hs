{-# LANGUAGE   MultiParamTypeClasses
             , FlexibleInstances
  #-}

{- |
   Module      : Data.GraphViz.Types.Generalised.
   Description : Alternate definition of the Graphviz types.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides an alternate definition of the types found in
   "Data.GraphViz.Types", in that the ordering constraint found in
   'DotStatements' is no longer present.  All other
   limitations\/constraints are still present however.

   The types here have the same names as those in
   "Data.GraphViz.Types" but with a prefix of @\"G\"@.

   This module is partially experimental, and may change in the
   future.
-}
module Data.GraphViz.Types.Generalised
       ( -- * The overall representation of a graph in generalised /Dot/ format.
         GDotGraph(..)
         -- * Sub-components of a @GDotGraph@.
       , GDotStatements
       , GDotStatement(..)
       , GDotSubGraph(..)
         -- ** Re-exported from @Data.GraphViz.Types@.
       , GraphID(..)
       , GlobalAttributes(..)
       , DotNode(..)
       , DotEdge(..)
         -- * Conversion from a @DotGraph@.
       , generaliseDotGraph
       ) where

import Data.GraphViz.Types hiding (GraphID(..), DotEdge(..))
import Data.GraphViz.Types.Common
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (><))
import qualified Data.Foldable as F
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | The internal representation of a generalised graph in Dot form.
data GDotGraph a = GDotGraph { gStrictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                             , gDirectedGraph   :: Bool
                             , gGraphID         :: Maybe GraphID
                             , gGraphStatements :: GDotStatements a
                             }
                 deriving (Eq, Ord, Show, Read)

instance (PrintDot n, ParseDot n) => DotRepr GDotGraph n where
  graphIsDirected = gDirectedGraph

  makeStrict g = g { gStrictGraph = True }

  setID i g = g { gGraphID = Just i }

  graphNodes = statementNodes . gGraphStatements

  graphEdges = statementEdges . gGraphStatements

instance (PrintDot a) => PrintDot (GDotGraph a) where
  unqtDot = printStmtBased printGraphID' gGraphStatements printGStmts
    where
      printGraphID' = printGraphID gStrictGraph gDirectedGraph gGraphID

instance (ParseDot a) => ParseDot (GDotGraph a) where
    parseUnqt = parseStmtBased parseGStmts (parseGraphID GDotGraph)

    parse = parseUnqt -- Don't want the option of quoting
            `adjustErr`
            (++ "\n\nNot a valid DotGraph")

instance Functor GDotGraph where
    fmap f g = g { gGraphStatements = (fmap . fmap) f $ gGraphStatements g }

-- | Convert a 'DotGraph' to a 'GDotGraph', keeping the same order of
--   statements.
generaliseDotGraph    :: DotGraph a -> GDotGraph a
generaliseDotGraph dg = GDotGraph { gStrictGraph = strictGraph dg
                                  , gDirectedGraph = directedGraph dg
                                  , gGraphID = graphID dg
                                  , gGraphStatements = generaliseStatements
                                                       $ graphStatements dg
                                  }

-- -----------------------------------------------------------------------------

type GDotStatements a = Seq (GDotStatement a)

printGStmts :: (PrintDot a) => GDotStatements a -> DotCode
printGStmts = vcat . map toDot . F.toList

parseGStmts :: (ParseDot a) => Parse (GDotStatements a)
parseGStmts = liftM (Seq.fromList . concat)
              $ sepBy (whitespace' >> p) statementEnd
                `discard`
                optional statementEnd
  where
    -- Have to do something special here because of "a -> b -> c"
    -- syntax for edges.
    p = liftM (map DE) parseEdgeLine
        `onFail`
        liftM return parse

statementNodes :: GDotStatements a -> [DotNode a]
statementNodes = concatMap stmtNodes . F.toList

statementEdges :: GDotStatements a -> [DotEdge a]
statementEdges = concatMap stmtEdges . F.toList

generaliseStatements       :: DotStatements a -> GDotStatements a
generaliseStatements stmts = atts >< sgs >< ns >< es
  where
    atts = Seq.fromList . map GA $ attrStmts stmts
    sgs = Seq.fromList . map (SG . generaliseSubGraph) $ subGraphs stmts
    ns = Seq.fromList . map DN $ nodeStmts stmts
    es = Seq.fromList . map DE $ edgeStmts stmts


data GDotStatement a = GA GlobalAttributes
                     | SG (GDotSubGraph a)
                     | DN (DotNode a)
                     | DE (DotEdge a)
                     deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (GDotStatement a) where
  unqtDot (GA ga) = unqtDot ga
  unqtDot (SG sg) = unqtDot sg
  unqtDot (DN dn) = unqtDot dn
  unqtDot (DE de) = unqtDot de

instance (ParseDot a) => ParseDot (GDotStatement a) where
  parseUnqt = oneOf [ liftM GA parseUnqt
                    , liftM SG parseUnqt
                    , liftM DN parseUnqt
                    , liftM DE parseUnqt
                    ]

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "Not a valid statement")

instance Functor GDotStatement where
  fmap _ (GA ga) = GA ga -- Have to re-make this to make the type checker happy.
  fmap f (SG sg) = SG $ fmap f sg
  fmap f (DN dn) = DN $ fmap f dn
  fmap f (DE de) = DE $ fmap f de

stmtNodes         :: GDotStatement a -> [DotNode a]
stmtNodes (SG sg) = subGraphNodes sg
stmtNodes (DN dn) = [dn]
stmtNodes _       = []

stmtEdges         :: (GDotStatement a) -> [DotEdge a]
stmtEdges (SG sg) = subGraphEdges sg
stmtEdges (DE de) = [de]
stmtEdges _       = []

-- -----------------------------------------------------------------------------

data GDotSubGraph a = GDotSG { gIsCluster     :: Bool
                             , gSubGraphID    :: Maybe GraphID
                             , gSubGraphStmts :: GDotStatements a
                             }
                    deriving (Eq, Ord, Show, Read)

instance (PrintDot a) => PrintDot (GDotSubGraph a) where
  unqtDot = printStmtBased printSubGraphID' gSubGraphStmts printGStmts

  unqtListToDot = printStmtBasedList printSubGraphID' gSubGraphStmts printGStmts

  listToDot = unqtListToDot

printSubGraphID' :: GDotSubGraph a -> DotCode
printSubGraphID' = printSubGraphID (\sg -> (gIsCluster sg, gSubGraphID sg))

instance (ParseDot a) => ParseDot (GDotSubGraph a) where
  parseUnqt = parseStmtBased parseGStmts (parseSubGraphID GDotSG)
              `onFail`
              -- Take anonymous GDotSubGraphs into account
              liftM (GDotSG False Nothing) (parseBracesBased parseGStmts)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          (++ "\n\nNot a valid Sub Graph")

  parseUnqtList = sepBy (whitespace' >> parseUnqt) newline'

  parseList = parseUnqtList

instance Functor GDotSubGraph where
    fmap f sg = sg { gSubGraphStmts = (fmap . fmap) f $ gSubGraphStmts sg }

subGraphNodes :: GDotSubGraph a -> [DotNode a]
subGraphNodes = statementNodes . gSubGraphStmts

subGraphEdges :: GDotSubGraph a -> [DotEdge a]
subGraphEdges = statementEdges . gSubGraphStmts

generaliseSubGraph                       :: DotSubGraph a -> GDotSubGraph a
generaliseSubGraph (DotSG isC mID stmts) = GDotSG { gIsCluster     = isC
                                                  , gSubGraphID    = mID
                                                  , gSubGraphStmts = stmts'
                                                  }
  where
    stmts' = generaliseStatements stmts
