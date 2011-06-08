{- |
   Module      : Data.GraphViz.Types.Canonical
   Description : The canonical representation of Dot graphs.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   A canonical Dot graph requires that within each graph/sub-graph,
   the statements are in the following order:

   * global attributes

   * sub-graphs/clusters

   * nodes

   * edges

   If you require arbitrary ordering of statements, then use
   "Data.GraphViz.Types.Generalised".

 -}
module Data.GraphViz.Types.Canonical
       ( DotGraph(..)
         -- * Sub-components of a @DotGraph@.
       , DotStatements(..)
       , DotSubGraph(..)
         -- * Re-exported from @Data.GraphViz.Types@
       , GraphID(..)
       , GlobalAttributes(..)
       , DotNode(..)
       , DotEdge(..)
       ) where

import Data.GraphViz.Types.Common
import Data.GraphViz.Attributes( Attribute
                               , usedByGraphs, usedByClusters, usedBySubGraphs)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing

import Control.Arrow((&&&))
import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | A Dot graph in canonical form.
data DotGraph a = DotGraph { strictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements a
                           }
                deriving (Eq, Ord, Show, Read)

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
