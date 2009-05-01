{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Types.Clustering
   Description : Definition of the clustering types for GraphViz.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines types for creating clusters.
-}

module Data.GraphViz.Types.Clustering
    ( NodeCluster(..)
    , clustersToNodes
    ) where

-- LT is defined in Attributes
import Prelude hiding (LT)
import qualified Prelude as P

import Data.GraphViz.Types
import Data.GraphViz.Attributes

import Data.List(groupBy, sortBy, mapAccumL)
import Data.Graph.Inductive.Graph(Graph, LNode, labNodes)

-- -----------------------------------------------------------------------------

-- | Define into which cluster a particular node belongs.
--   Nodes can be nested to arbitrary depth.
data NodeCluster c a = N (LNode a) | C c (NodeCluster c a)
                        deriving (Show)

-- | Create the @'DotNode'@s for the given graph.
clustersToNodes :: (Ord c, Graph gr) => (LNode a -> NodeCluster c a)
                   -> (c -> [Attribute]) -> (LNode a -> [Attribute])
                   -> gr a b -> [DotNode]
clustersToNodes clusterBy fmtCluster fmtNode
    = treesToNodes fmtCluster fmtNode
      . collapseNClusts
      . map (clustToTree . clusterBy)
      . labNodes

-- -----------------------------------------------------------------------------

-- | A tree representation of a cluster.
data ClusterTree c a = NT (LNode a) | CT c [ClusterTree c a]
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
clustOrder (NT _)    (CT _ _)  = P.LT -- don't use the attribute LT
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
      grpCls ns@((NT _):_)   = ns
      grpCls cs@((CT c _):_) = [CT c (collapseNClusts $ concatMap getNodes cs)]


-- | Convert the cluster representation of the trees into @'DotNode'@s.
--   Clusters will be labelled with @'Int'@s.
treesToNodes :: (c -> [Attribute]) -> (LNode a -> [Attribute])
             -> [ClusterTree c a] -> [DotNode]
treesToNodes fmtCluster fmtNode = snd . treesToNodesFrom fmtCluster fmtNode 0

-- | Start labelling the clusters with this @'Int'@.
treesToNodesFrom :: (c -> [Attribute]) -> (LNode a -> [Attribute])
                 -> Int -> [ClusterTree c a] -> (Int,[DotNode])
treesToNodesFrom fmtCluster fmtNode n = mapAccumL mkNodes n
    where
      mkNodes = treeToNode fmtCluster fmtNode

-- | Convert this 'ClusterTree' into its 'DotNode' representation.
treeToNode :: (c -> [Attribute]) -> (LNode a -> [Attribute])
           -> Int -> ClusterTree c a -> (Int, DotNode)
treeToNode _ fmtNode n (NT ln) = ( n
                                 , DotNode { nodeID = fst ln
                                           , nodeAttributes = fmtNode ln
                                           }
                                 )

treeToNode fmtCluster fmtNode n (CT c nts) = (n',clust)
    where
      (n', nts') = treesToNodesFrom fmtCluster fmtNode (n+1) nts
      clust = DotCluster { clusterID = show n
                         , clusterAttributes = fmtCluster c
                         , clusterElems = nts'
                         }
