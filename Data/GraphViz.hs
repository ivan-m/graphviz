{-# LANGUAGE RecordPuns 
           , ScopedTypeVariables
           #-}

 {- GraphViz ------------------------------------------------------\
 |                                                                 |
 | Copyright (c) 2008, Matthew Sackman (matthew@wellquite.org)     |
 |                                                                 |
 | DisTract is freely distributable under the terms of a 3-Clause  |
 | BSD-style license.                                              |
 |                                                                 |
 \-----------------------------------------------------------------}

module Data.GraphViz
    ( graphToDot
    , graphToGraph
    , readDotGraph
    , DotGraph (..)
    , DotNode (..)
    , DotEdge (..)
    , AttributeNode
    , AttributeEdge
    , module Data.GraphViz.Attributes
    )
    where

import Data.Graph.Inductive.Graph
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
                         }

data DotNode = DotNode { nodeID :: Int
                       , nodeAttributes :: [Attribute]
                       }

data DotEdge = DotEdge { edgeHeadNodeID :: Int
                       , edgeTailNodeID :: Int
                       , edgeAttributes :: [Attribute]
                       }

instance Show DotNode where
    show (DotNode { nodeID, nodeAttributes })
        | null nodeAttributes = '\t':((show nodeID) ++ ";")
        | otherwise = '\t':((show nodeID) ++ (' ':((show nodeAttributes) ++ ";")))

instance Show DotEdge where
    show (DotEdge { edgeHeadNodeID, edgeTailNodeID, edgeAttributes })
        = '\t' : ((show edgeTailNodeID) ++ " -> " ++ (show edgeHeadNodeID) ++ attributes)
          where
            attributes = case edgeAttributes of
                           [] -> ";"
                           a -> ' ':((show a) ++ ";")

instance Show DotGraph where
    show (DotGraph { graphAttributes, graphNodes, graphEdges })
        = unlines $ "digraph {" : (rest ++ ["}"])
        where
          rest = case graphAttributes of
                   [] -> nodesEdges
                   a -> ("\tgraph " ++ (show a) ++ ";") : nodesEdges
          nodesEdges = (map show graphNodes) ++ (map show graphEdges)

-- | Convert a graph to dot format. You can then write this to a file
--   and run dot on it
graphToDot :: (Graph gr) => gr a b -> [Attribute] -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> DotGraph
graphToDot graph graphAttributes fmtNode fmtEdge
    = DotGraph { graphAttributes, graphNodes, graphEdges }
      where
        graphNodes = map mkDotNode . labNodes $ graph
        graphEdges = map mkDotEdge . labEdges $ graph
        mkDotNode n = DotNode { nodeID = fst n, nodeAttributes = fmtNode n }
        mkDotEdge e@(f, t, _) = DotEdge { edgeHeadNodeID = t, edgeTailNodeID = f, edgeAttributes = fmtEdge e }

type AttributeNode a = ([Attribute], a)
type AttributeEdge b = ([Attribute], b)

-- | Run the graph via dot to get positional information and then
--   combine that information back into the original graph
graphToGraph :: forall gr a b . (Graph gr) =>
                gr a b -> [Attribute] -> (LNode a -> [Attribute]) -> (LEdge b -> [Attribute]) -> IO (gr (AttributeNode a) (AttributeEdge b))
graphToGraph gr graphAttributes fmtNode fmtEdge
    = do { (inp, outp, errp, proc) <- runInteractiveCommand "dot -Tdot"
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
      dot = graphToDot gr graphAttributes fmtNode fmtEdge
      rebuildGraphWithAttributes :: String -> gr (AttributeNode a) (AttributeEdge b)
      rebuildGraphWithAttributes dotResult = mkGraph lnodes ledges
          where
            lnodes = map (\(n, l) -> (n, (fromJust $ Map.lookup n nodeMap, l))) . labNodes $ gr
            ledges = map (\(f,t, l) -> (f, t, (fromJust $ Map.lookup (f,t) edgeMap, l))) . labEdges $ gr
            (DotGraph { graphEdges, graphNodes }) = fst . runParser readDotGraph $ dotResult
            nodeMap = Map.fromList . map (\n -> (nodeID n, nodeAttributes n)) $ graphNodes
            edgeMap = Map.fromList . map (\e -> ((edgeTailNodeID e, edgeHeadNodeID e), edgeAttributes e)) $ graphEdges

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
                 ; string "->"
                 ; whitespace
                 ; edgeHeadNodeID <- number
                 ; as <- optional (whitespace >> readAttributesList)
                 ; char ';'
                 ; skipToNewline
                 ; return (DotEdge { edgeHeadNodeID, edgeTailNodeID, edgeAttributes = fromMaybe [] as })
                 }

readDotGraph :: Parser Char DotGraph
readDotGraph = do { string "digraph"
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
                  ; return $ DotGraph { graphAttributes, graphNodes, graphEdges }
                  }
