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
    ( NodeCluster(..)
    , clustersToNodes
    ) where

import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete(Attributes)

import Data.Either(partitionEithers)
import Data.List(groupBy, sortBy)

-- -----------------------------------------------------------------------------

-- | Define into which cluster a particular node belongs.
--   Clusters can be nested to arbitrary depth.
data NodeCluster c a = N a -- ^ Indicates the actual Node in the Graph.
                     | C c (NodeCluster c a) -- ^ Indicates that the
                                             --   'NodeCluster' is in
                                             --   the Cluster /c/.
                        deriving (Show)

-- | Extract the clusters and nodes from the list of nodes.
clustersToNodes :: (Ord c) => ((n,a) -> NodeCluster c (n,l))
                  -> (c -> Bool) -> (c -> GraphID) -> (c -> [GlobalAttributes])
                  -> ((n,l) -> Attributes) -> [(n,a)]
                  -> ([DotSubGraph n], [DotNode n])
clustersToNodes clusterBy isC cID fmtCluster fmtNode
    = treesToDot isC cID fmtCluster fmtNode
      . collapseNClusts
      . map (clustToTree . clusterBy)

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
treesToDot :: (c -> Bool) -> (c -> GraphID) -> (c -> [GlobalAttributes])
              -> ((n,a) -> Attributes) -> [ClusterTree c (n,a)]
              -> ([DotSubGraph n], [DotNode n])
treesToDot isC cID fmtCluster fmtNode
    = partitionEithers
      . map (treeToDot isC cID fmtCluster fmtNode)

-- | Convert this 'ClusterTree' into its /Dot/ representation.
treeToDot :: (c -> Bool) -> (c -> GraphID) -> (c -> [GlobalAttributes])
             -> ((n,a) -> Attributes) -> ClusterTree c (n,a)
             -> Either (DotSubGraph n) (DotNode n)
treeToDot _ _ _ fmtNode (NT ln)
    = Right DotNode { nodeID         = fst ln
                    , nodeAttributes = fmtNode ln
                    }
treeToDot isC cID fmtCluster fmtNode (CT c nts)
    = Left DotSG { isCluster     = isC c
                 , subGraphID    = Just $ cID c
                 , subGraphStmts = stmts
                 }
  where
    stmts = DotStmts { attrStmts = fmtCluster c
                     , subGraphs = cs
                     , nodeStmts = ns
                     , edgeStmts = []
                     }
    (cs, ns) = treesToDot isC cID fmtCluster fmtNode nts
