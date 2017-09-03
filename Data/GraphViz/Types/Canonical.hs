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

   This Dot graph representation is ideally suited for converting
   other data structures to Dot form (especially with the help of
   @graphElemsToDot@ from "Data.GraphViz").

   If you require arbitrary ordering of statements, then use
   "Data.GraphViz.Types.Generalised".

   The sample graph could be implemented (this is actually the result
   of calling @canonicalise@ from "Data.GraphViz.Algorithms" on the
   generalised one) as:

   > DotGraph { strictGraph = False
   >          , directedGraph = True
   >          , graphID = Just (Str "G")
   >          , graphStatements = DotStmts { attrStmts = []
   >                                       , subGraphs = [ DotSG { isCluster = True
   >                                                             , subGraphID = Just (Num (Int 0))
   >                                                             , subGraphStmts = DotStmts { attrStmts = [ GraphAttrs [ style filled
   >                                                                                                                   , color LightGray
   >                                                                                                                   , textLabel "process #1"]
   >                                                                                                      , NodeAttrs [style filled, color White]]
   >                                                                                        , subGraphs = []
   >                                                                                        , nodeStmts = [ DotNode "a0" []
   >                                                                                                      , DotNode "a1" []
   >                                                                                                      , DotNode "a2" []
   >                                                                                                      , DotNode "a3" []]
   >                                                                                        , edgeStmts = [ DotEdge "a0" "a1" []
   >                                                                                                      , DotEdge "a1" "a2" []
   >                                                                                                      , DotEdge "a2" "a3" []
   >                                                                                                      , DotEdge "a3" "a0" []]}}
   >                                                     , DotSG { isCluster = True
   >                                                             , subGraphID = Just (Num (Int 1))
   >                                                             , subGraphStmts = DotStmts { attrStmts = [ GraphAttrs [textLabel "process #2", color Blue]
   >                                                                                                      , NodeAttrs [style filled]]
   >                                                                                        , subGraphs = []
   >                                                                                        , nodeStmts = [ DotNode "b0" []
   >                                                                                                      , DotNode "b1" []
   >                                                                                                      , DotNode "b2" []
   >                                                                                                      , DotNode "b3" []]
   >                                                                                        , edgeStmts = [ DotEdge "b0" "b1" []
   >                                                                                                      , DotEdge "b1" "b2" []
   >                                                                                                      , DotEdge "b2" "b3" []]}}]
   >                                       , nodeStmts = [ DotNode "end" [shape MSquare]
   >                                                     , DotNode "start" [shape MDiamond]]
   >                                       , edgeStmts = [ DotEdge "start" "a0" []
   >                                                     , DotEdge "start" "b0" []
   >                                                     , DotEdge "a1" "b3" []
   >                                                     , DotEdge "b2" "a3" []
   >                                                     , DotEdge "a3" "end" []
   >                                                     , DotEdge "b3" "end" []]}}

   Note that whilst the above graph represents the same Dot graph as
   specified in "Data.GraphViz.Types.Generalised", etc., it /may/ be
   drawn slightly differently by the various Graphviz tools.

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

import Data.GraphViz.Internal.State        (AttributeType (..))
import Data.GraphViz.Internal.Util         (bool)
import Data.GraphViz.Parsing
import Data.GraphViz.Printing
import Data.GraphViz.Types.Internal.Common

import Control.Arrow ((&&&))

-- -----------------------------------------------------------------------------

-- | A Dot graph in canonical form.
data DotGraph n = DotGraph { strictGraph     :: Bool  -- ^ If 'True', no multiple edges are drawn.
                           , directedGraph   :: Bool
                           , graphID         :: Maybe GraphID
                           , graphStatements :: DotStatements n
                           }
                deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotGraph n) where
  unqtDot = printStmtBased printGraphID' (const GraphAttribute)
                           graphStatements toDot
    where
      printGraphID' = printGraphID strictGraph directedGraph graphID

instance (ParseDot n) => ParseDot (DotGraph n) where
  parseUnqt = parseGraphID DotGraph
              <*> parseBracesBased GraphAttribute parseUnqt

  parse = parseUnqt -- Don't want the option of quoting

-- | Assumed to be an injective mapping function.
instance Functor DotGraph where
  fmap f g = g { graphStatements = fmap f $ graphStatements g }

-- -----------------------------------------------------------------------------

data DotStatements n = DotStmts { attrStmts :: [GlobalAttributes]
                                , subGraphs :: [DotSubGraph n]
                                , nodeStmts :: [DotNode n]
                                , edgeStmts :: [DotEdge n]
                                }
                     deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotStatements n) where
  unqtDot stmts = vcat $ sequence [ unqtDot $ attrStmts stmts
                                  , unqtDot $ subGraphs stmts
                                  , unqtDot $ nodeStmts stmts
                                  , unqtDot $ edgeStmts stmts
                                  ]

instance (ParseDot n) => ParseDot (DotStatements n) where
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
          ("Not a valid set of statements\n\t"++)

instance Functor DotStatements where
  fmap f stmts = stmts { subGraphs = map (fmap f) $ subGraphs stmts
                       , nodeStmts = map (fmap f) $ nodeStmts stmts
                       , edgeStmts = map (fmap f) $ edgeStmts stmts
                       }

-- -----------------------------------------------------------------------------

data DotSubGraph n = DotSG { isCluster     :: Bool
                           , subGraphID    :: Maybe GraphID
                           , subGraphStmts :: DotStatements n
                           }
                   deriving (Eq, Ord, Show, Read)

instance (PrintDot n) => PrintDot (DotSubGraph n) where
  unqtDot = printStmtBased printSubGraphID' subGraphAttrType
                           subGraphStmts toDot

  unqtListToDot = printStmtBasedList printSubGraphID' subGraphAttrType
                                     subGraphStmts toDot

  listToDot = unqtListToDot

subGraphAttrType :: DotSubGraph n -> AttributeType
subGraphAttrType = bool SubGraphAttribute ClusterAttribute . isCluster

printSubGraphID' :: DotSubGraph n -> DotCode
printSubGraphID' = printSubGraphID (isCluster &&& subGraphID)

instance (ParseDot n) => ParseDot (DotSubGraph n) where
  parseUnqt = parseSubGraph DotSG parseUnqt
              `onFail`
              -- Take "anonymous" DotSubGraphs into account.
              fmap (DotSG False Nothing)
                   (parseBracesBased SubGraphAttribute parseUnqt)

  parse = parseUnqt -- Don't want the option of quoting
          `adjustErr`
          ("Not a valid Sub Graph\n\t"++)

  parseUnqtList = sepBy (whitespace >> parseUnqt) newline'

  parseList = parseUnqtList

instance Functor DotSubGraph where
  fmap f sg = sg { subGraphStmts = fmap f $ subGraphStmts sg }
