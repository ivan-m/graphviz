{-# LANGUAGE RecordPuns
           , ScopedTypeVariables
           #-}

 {--GraphViz ------------------------------------------------------\
 |                                                                 |
 | Copyright (c) 2008, Matthew Sackman (matthew@wellquite.org),    |
 |              Ivan Lazar Miljenovic (ivan.miljenovic@gmail.com)  |
 |                                                                 |
 | GraphViz is freely distributable under the terms of a 3-Clause  |
 | BSD-style license.                                              |
 |                                                                 |
 \-----------------------------------------------------------------}

module Data.GraphViz
    ( graphToDot
    , clusterGraphToDot
    , graphToGraph
    , readDotGraph
    , commandFor
    , DotGraph (..)
    , DotNode (..)
    , DotEdge (..)
    , NodeCluster(..)
    , AttributeNode
    , AttributeEdge
    , module Data.GraphViz.Attributes
    )
    where

-- LT is defined in Attributes
import Prelude hiding (LT)
import qualified Prelude as P

import Data.Graph.Inductive.Graph
import Data.List
import Data.Function
import qualified Data.Set as Set
import Text.ParserCombinators.PolyLazy
import System.IO
import System.Process
import Control.Concurrent
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

import Data.GraphViz.Attributes
import Data.GraphViz.ParserCombinators

data DotGraph = DotGraph { graphAttributes :: [Attribute]
                         , graphNodes :: [DotNode]
                         , graphEdges :: [DotEdge]
                         , directedGraph :: Bool
                         }

data DotNode
    = DotNode { nodeID :: Int
              , nodeAttributes :: [Attribute]
              }
    | DotCluster { clusterID         :: String
                 , clusterAttributes :: [Attribute]
                 , clusterElems      :: [DotNode]
                 }

data DotEdge = DotEdge { edgeHeadNodeID :: Int
                       , edgeTailNodeID :: Int
                       , edgeAttributes :: [Attribute]
                       , directedEdge   :: Bool
                       }

instance Show DotNode where
    show n = init . unlines . addTabs $ nodesToString n

nodesToString :: DotNode -> [String]
nodesToString (DotNode { nodeID, nodeAttributes })
    | null nodeAttributes = [nID ++ ";"]
    | otherwise           = [nID ++ (' ':((show nodeAttributes) ++ ";"))]
    where
      nID = show nodeID
nodesToString (DotCluster { clusterID, clusterAttributes, clusterElems })
    = ["subgraph cluster_" ++ clusterID ++ " {"] ++ (addTabs inner) ++ ["}"]
    where
      inner = case clusterAttributes of
                [] -> nodes
                a  -> ("graph " ++ (show a) ++ ";") : nodes
      nodes = concatMap nodesToString clusterElems

addTabs :: [String] -> [String]
addTabs = map ('\t':)

instance Show DotEdge where
    show (DotEdge { edgeHeadNodeID, edgeTailNodeID, edgeAttributes, directedEdge })
        = '\t' : ((show edgeTailNodeID) ++ edge ++ (show edgeHeadNodeID) ++ attributes)
          where
            edge = " " ++ (if directedEdge then dirEdge else undirEdge) ++ " "
            attributes = case edgeAttributes of
                           [] -> ";"
                           a  -> ' ':((show a) ++ ";")

instance Show DotGraph where
    show (DotGraph { graphAttributes, graphNodes, graphEdges, directedGraph })
        = unlines $ gType : " {" : (rest ++ ["}"])
        where
          gType = if directedGraph then dirGraph else undirGraph
          rest = case graphAttributes of
                   [] -> nodesEdges
                   a -> ("\tgraph " ++ (show a) ++ ";") : nodesEdges
          nodesEdges = (map show graphNodes) ++ (map show graphEdges)

-- | Define into which cluster a particular node belongs.
--   Nodes can be nested to arbitrary depth.
data NodeCluster c a = N (LNode a) | C c (NodeCluster c a)
                        deriving (Show)

-- | A tree representation of a cluster.
data ClusterTree c a = NT (LNode a) | CT c [ClusterTree c a]
                       deriving (Show)

-- Convert a single node cluster into its tree representation.
clustToTree          :: NodeCluster c a -> ClusterTree c a
clustToTree (N ln)   = NT ln
clustToTree (C c nc) = CT c [clustToTree nc]

-- Two nodes are in the same "default" cluster; otherwise check if they
-- are in the same cluster.
sameClust :: (Eq c) => ClusterTree c a -> ClusterTree c a -> Bool
sameClust (NT _)    (NT _)    = True
sameClust (CT c1 _) (CT c2 _) = c1 == c2
sameClust _         _         = False

-- Singleton nodes come first, and then ordering based upon the cluster.
clustOrder :: (Ord c) => ClusterTree c a -> ClusterTree c a -> Ordering
clustOrder (NT _)    (NT _)    = EQ
clustOrder (NT _)    (CT _ _)  = P.LT -- don't use the attribute LT
clustOrder (CT _ _)  (NT _)    = GT
clustOrder (CT c1 _) (CT c2 _) = compare c1 c2

-- Extract the sub-trees.
getNodes           :: ClusterTree c a -> [ClusterTree c a]
getNodes n@(NT _)  = [n]
getNodes (CT _ ns) = ns

-- Combine clusters.
collapseNClusts :: (Ord c) => [ClusterTree c a] -> [ClusterTree c a]
collapseNClusts = concatMap grpCls
                  . groupBy sameClust
                  . sortBy clustOrder
    where
      grpCls []              = []
      grpCls ns@((NT _):_)   = ns
      grpCls cs@((CT c _):_) = [CT c (collapseNClusts $ concatMap getNodes cs)]

-- Differences between directed and undirected graphs.

dirEdge, undirEdge :: String
dirEdge = "->"
undirEdge = "--"

dirGraph, undirGraph :: String
dirGraph = "digraph"
undirGraph = "graph"

dirCommand, undirCommand :: String
dirCommand = "dot"
undirCommand = "neato"

-- | The appropriate GraphViz command for the given graph.
commandFor    :: DotGraph -> String
commandFor dg = if (directedGraph dg)
                then dirCommand
                else undirCommand

-- Determine ifi the given graph is undirected or directed.
isUndir   :: (Ord b, Graph g) => g a b -> Bool
isUndir g = all hasFlip edges
    where
      edges = labEdges g
      eSet = Set.fromList edges
      hasFlip e = Set.member (flippedEdge e) eSet
      flippedEdge (f,t,l) = (t,f,l)

-- | Convert a graph to dot format. You can then write this to a file
--   and run the appropriate command on it (found using 'commandFor').
graphToDot :: (Ord b, Graph gr) => gr a b -> [Attribute]
           -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> DotGraph
graphToDot graph graphAttributes fmtNode fmtEdge
    = clusterGraphToDot graph graphAttributes clusterBy fmtCluster fmtNode fmtEdge
      where
        clusterBy :: LNode a -> NodeCluster () a
        clusterBy = N
        fmtCluster _ = []

-- | Convert a graph to dot format, using the specified clustering function
--   to group nodes into clusters.  You can then write this to a file and
--   run the appropriate command on it (found using 'commandFor').
--   Clusters can be nested to arbitrary depth.
clusterGraphToDot :: (Ord c, Ord b, Graph gr) => gr a b
                  -> [Attribute] -> (LNode a -> NodeCluster c a)
                  -> (c -> [Attribute]) -> (LNode a -> [Attribute])
                  -> (LEdge b -> [Attribute]) -> DotGraph
clusterGraphToDot graph graphAttributes clusterBy fmtCluster fmtNode fmtEdge
    = DotGraph { graphAttributes, graphNodes, graphEdges, directedGraph }
      where
        clusters = collapseNClusts . map (clustToTree . clusterBy) $ labNodes graph
        graphNodes = treesToNodes fmtCluster fmtNode clusters
        directedGraph = not $ isUndir graph
        graphEdges = catMaybes . map mkDotEdge . labEdges $ graph
        mkDotEdge e@(f,t,_) = if (directedGraph || f <= t)
                              then Just $ DotEdge {edgeHeadNodeID = t
                                                  ,edgeTailNodeID = f
                                                  ,edgeAttributes = fmtEdge e
                                                  ,directedEdge = directedGraph}
                              else Nothing

-- Convert the cluster representation of the trees into DotNodes.
-- Clusters will be labelled with integers.
treesToNodes :: (c -> [Attribute]) -> (LNode a -> [Attribute])
             -> [ClusterTree c a] -> [DotNode]
treesToNodes fmtCluster fmtNode = snd . treesToNodesFrom fmtCluster fmtNode 0

-- Start labelling the clusters with this integer.
treesToNodesFrom :: (c -> [Attribute]) -> (LNode a -> [Attribute])
                 -> Int -> [ClusterTree c a] -> (Int,[DotNode])
treesToNodesFrom fmtCluster fmtNode n = mapAccumL mkNodes n
    where
      mkNodes = treeToNode fmtCluster fmtNode

-- Convert this ClusterTree into its DotNode representation.
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

type AttributeNode a = ([Attribute], a)
type AttributeEdge b = ([Attribute], b)

-- | Run the graph via dot to get positional information and then
--   combine that information back into the original graph.
--   Note that this doesn't support graphs with clusters.
graphToGraph :: forall gr a b . (Ord b, Graph gr) =>
                gr a b -> [Attribute] -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph gr graphAttributes fmtNode fmtEdge
    = do { (inp, outp, errp, proc) <- runInteractiveCommand (command++" -Tdot")
         ; hPutStr inp (show dot)
         ; hClose inp
         ; forkIO $ (hGetContents errp >>= hPutStr stderr)
         ; res <- hGetContents outp
         ; (length res) `seq` return ()
         ; hClose outp
         ; hClose errp
         ; waitForProcess proc
         ; return $ rebuildGraphWithAttributes res
         }
    where
      undirected = isUndir gr
      command = if undirected then undirCommand else dirCommand
      dot = graphToDot gr graphAttributes fmtNode fmtEdge
      rebuildGraphWithAttributes :: String -> gr (AttributeNode a) (AttributeEdge b)
      rebuildGraphWithAttributes dotResult = mkGraph lnodes ledges
          where
            lnodes = map (\(n, l) -> (n, (fromJust $ Map.lookup n nodeMap, l))) . labNodes $ gr
            ledges = map createEdges . labEdges $ gr
            (DotGraph { graphEdges, graphNodes }) = fst . runParser readDotGraph $ dotResult
            nodeMap = Map.fromList . map (\n -> (nodeID n, nodeAttributes n)) $ graphNodes
            edgeMap = Map.fromList . map (\e -> ((edgeTailNodeID e, edgeHeadNodeID e), edgeAttributes e)) $ graphEdges
            createEdges (f,t,l) = if (undirected && f > t)
                                  then (f,t,getLabel (t,f))
                                  else (f,t,getLabel (f,t))
                where
                  getLabel c = (fromJust $ Map.lookup c edgeMap,l)

readDotNode :: Parser Char DotNode
readDotNode = do { optional whitespace
                 ; nodeID <- number
                 ; as <- optional (whitespace >> readAttributesList)
                 ; char ';'
                 ; skipToNewline
                 ; return (DotNode { nodeID, nodeAttributes = fromMaybe [] as })
                 }

readDotEdge :: Parser Char DotEdge
readDotEdge = do { optional whitespace
                 ; edgeTailNodeID <- number
                 ; whitespace
                 ; edge <- strings [dirEdge,undirEdge]
                 ; whitespace
                 ; edgeHeadNodeID <- number
                 ; as <- optional (whitespace >> readAttributesList)
                 ; char ';'
                 ; skipToNewline
                 ; return (DotEdge { edgeHeadNodeID
                                   , edgeTailNodeID
                                   , edgeAttributes = fromMaybe [] as
                                   , directedEdge = edge == dirEdge })
                 }
    where


readDotGraph :: Parser Char DotGraph
readDotGraph = do { d <- strings [dirGraph,undirGraph]
                  ; let directedGraph = d == dirGraph
                  ; whitespace
                  ; char '{'
                  ; skipToNewline
                  ; graphAttributes
                      <- liftM concat $
                         many (optional whitespace >>
                               oneOf [ (string "edge" >> skipToNewline >> return [])
                                     , (string "node" >> skipToNewline >> return [])
                                     , (string "graph" >> whitespace >> readAttributesList >>= \as -> skipToNewline >> return as)
                                     ]
                              )
                  ; graphNodes <- many readDotNode
                  ; graphEdges <- many readDotEdge
                  ; char '}'
                  ; return $ DotGraph { graphAttributes, graphNodes, graphEdges, directedGraph }
                  }
