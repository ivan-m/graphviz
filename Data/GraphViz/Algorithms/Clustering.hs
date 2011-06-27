{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Algorithms.Clustering
   Description : Definition of the clustering types for Graphviz.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines types for creating clusters.
-}
module Data.GraphViz.Algorithms.Clustering
    ( LNodeCluster
    , NodeCluster(..)
    , clustersToNodes
    ) where

import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete(Attributes)

import Data.Either(partitionEithers)
import Data.List(groupBy, sortBy)
import Data.Graph.Inductive.Graph(Graph, Node, LNode, labNodes)

-- -----------------------------------------------------------------------------

-- | A type alias for 'NodeCluster' that specifies that the node value
--   is an 'LNode'.
type LNodeCluster c a = NodeCluster c (LNode a)

-- | Define into which cluster a particular node belongs.
--   Clusters can be nested to arbitrary depth.
data NodeCluster c a = N a -- ^ Indicates the actual Node in the Graph.
                     | C c (NodeCluster c a) -- ^ Indicates that the
                                             --   'NodeCluster' is in
                                             --   the Cluster /c/.
                        deriving (Show)

-- | Create the /Dot/ representation for the given graph.
clustersToNodes :: (Ord c, Graph gr) => (LNode a -> NodeCluster c (LNode l))
                   -> (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
                   -> (LNode l -> Attributes) -> gr a b
                   -> ([DotSubGraph Node], [DotNode Node])
clustersToNodes clusterBy cID fmtCluster fmtNode
    = treesToDot cID fmtCluster fmtNode
      . collapseNClusts
      . map (clustToTree . clusterBy)
      . labNodes

-- -----------------------------------------------------------------------------

-- | A tree representation of a cluster.
data ClusterTree c a = NT a
                     | CT c [ClusterTree c a]
                     deriving (Show)

-- | Convert a single node cluster into its tree representation.
clustToTree          :: NodeCluster c a -> ClusterTree c a
clustToTree (N ln)   = NT ln
clustToTree (C c nc) = CT c [clustToTree nc]

-- | Two nodes are in the same "default" cluster; otherwise check if they
--   are in the same cluster.
sameClust :: (Eq c) => ClusterTree c a -> ClusterTree c a -> Bool
sameClust (NT _)    (NT _)    = True
sameClust (CT c1 _) (CT c2 _) = c1 == c2
sameClust _         _         = False

-- | Singleton nodes come first, and then ordering based upon the cluster.
clustOrder :: (Ord c) => ClusterTree c a -> ClusterTree c a -> Ordering
clustOrder (NT _)    (NT _)    = EQ
clustOrder (NT _)    (CT _ _)  = LT
clustOrder (CT _ _)  (NT _)    = GT
clustOrder (CT c1 _) (CT c2 _) = compare c1 c2

-- | Extract the sub-trees.
getNodes           :: ClusterTree c a -> [ClusterTree c a]
getNodes n@(NT _)  = [n]
getNodes (CT _ ns) = ns

-- | Combine clusters.
collapseNClusts :: (Ord c) => [ClusterTree c a] -> [ClusterTree c a]
collapseNClusts = concatMap grpCls
                  . groupBy sameClust
                  . sortBy clustOrder
  where
    grpCls []              = []
    grpCls ns@(NT _ : _)   = ns
    grpCls cs@(CT c _ : _) = [CT c (collapseNClusts $ concatMap getNodes cs)]

-- | Convert the cluster representation of the trees into 'DotNode's
--   and 'DotSubGraph's (with @'isCluster' = 'True'@, and
--   @'subGraphID' = 'Nothing'@).
treesToDot :: (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
              -> (LNode a -> Attributes) -> [ClusterTree c (LNode a)]
              -> ([DotSubGraph Node], [DotNode Node])
treesToDot cID fmtCluster fmtNode
    = partitionEithers
      . map (treeToDot cID fmtCluster fmtNode)

-- | Convert this 'ClusterTree' into its /Dot/ representation.
treeToDot :: (c -> Maybe GraphID) -> (c -> [GlobalAttributes])
             -> (LNode a -> Attributes) -> ClusterTree c (LNode a)
             -> Either (DotSubGraph Node) (DotNode Node)
treeToDot _ _ fmtNode (NT ln)
    = Right DotNode { nodeID         = fst ln
                    , nodeAttributes = fmtNode ln
                    }
treeToDot cID fmtCluster fmtNode (CT c nts)
    = Left DotSG { isCluster     = True
                 , subGraphID    = cID c
                 , subGraphStmts = stmts
                 }
  where
    stmts = DotStmts { attrStmts = fmtCluster c
                     , subGraphs = cs
                     , nodeStmts = ns
                     , edgeStmts = []
                     }
    (cs, ns) = treesToDot cID fmtCluster fmtNode nts
