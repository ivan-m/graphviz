{-# LANGUAGE NamedFieldPuns
           , ScopedTypeVariables
           #-}

{- |
   Module      : Data.GraphViz.Types
   Description : Definition of the GraphViz types.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the overall types and methods that interact
   with them for the GraphViz library.
-}

module Data.GraphViz.Types
    ( DotGraph(..)
    , DotNode(..)
    , DotEdge(..)
    , readDotGraph
    ) where

import Data.Maybe
import Control.Monad
import Text.ParserCombinators.Poly.Lazy

import Data.GraphViz.Attributes
import Data.GraphViz.ParserCombinators

-- -----------------------------------------------------------------------------

-- | The internal representation of a graph in Dot form.
data DotGraph = DotGraph { graphAttributes :: [Attribute]
                         , graphNodes :: [DotNode]
                         , graphEdges :: [DotEdge]
                         , directedGraph :: Bool
                         }

instance Show DotGraph where
    show (DotGraph { graphAttributes, graphNodes, graphEdges, directedGraph })
        = unlines $ gType : " {" : (rest ++ ["}"])
        where
          gType = if directedGraph then dirGraph else undirGraph
          rest = case graphAttributes of
                   [] -> nodesEdges
                   a -> ("\tgraph " ++ (show a) ++ ";") : nodesEdges
          nodesEdges = (map show graphNodes) ++ (map show graphEdges)

dirGraph :: String
dirGraph = "digraph"

undirGraph :: String
undirGraph = "graph"

-- -----------------------------------------------------------------------------

-- | A node in 'DotGraph' is either a singular node, or a cluster
--   containing nodes (or more clusters) within it.
data DotNode
    = DotNode { nodeID :: Int
              , nodeAttributes :: [Attribute]
              }
    | DotCluster { clusterID         :: String
                 , clusterAttributes :: [Attribute]
                 , clusterElems      :: [DotNode]
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

-- | Prefix each 'String' with a tab character.
addTabs :: [String] -> [String]
addTabs = map ('\t':)

-- -----------------------------------------------------------------------------

-- | An edge in 'DotGraph'.
data DotEdge = DotEdge { edgeHeadNodeID :: Int
                       , edgeTailNodeID :: Int
                       , edgeAttributes :: [Attribute]
                       , directedEdge   :: Bool
                       }

instance Show DotEdge where
    show (DotEdge { edgeHeadNodeID, edgeTailNodeID, edgeAttributes, directedEdge })
        = '\t' : ((show edgeTailNodeID) ++ edge ++ (show edgeHeadNodeID) ++ attributes)
          where
            edge = " " ++ (if directedEdge then dirEdge else undirEdge) ++ " "
            attributes = case edgeAttributes of
                           [] -> ";"
                           a  -> ' ':((show a) ++ ";")

dirEdge :: String
dirEdge = "->"

undirEdge :: String
undirEdge = "--"

-- -----------------------------------------------------------------------------

-- | Parse a 'DotNode'
readDotNode :: Parser Char DotNode
readDotNode = do { optional whitespace
                 ; nodeID <- number
                 ; as <- optional (whitespace >> readAttributesList)
                 ; char ';'
                 ; skipToNewline
                 ; return (DotNode { nodeID, nodeAttributes = fromMaybe [] as })
                 }

-- | Parse a 'DotEdge'
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

-- | Parse a 'DotGraph'
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
