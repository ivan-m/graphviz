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

import Data.Char(isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe
import Control.Monad
import Text.ParserCombinators.Poly.Lazy

import Data.GraphViz.Attributes
import Data.GraphViz.ParserCombinators

-- -----------------------------------------------------------------------------

-- | The internal representation of a graph in Dot form.
data DotGraph = DotGraph { strictGraph     :: Bool
                         , directedGraph   :: Bool
                         , graphID         :: Maybe GraphID
                         , graphAttributes :: [Attribute]
                         , graphNodes      :: [DotNode]
                         , graphEdges      :: [DotEdge]
                         }
--                deriving (Show, Read)

instance Show DotGraph where
    show g
        = unlines $ gType : " {" : (rest ++ ["}"])
        where
          gType = if directedGraph g then dirGraph else undirGraph
          rest = case graphAttributes g of
                   [] -> nodesEdges
                   a -> ("\tgraph " ++ show a ++ ";") : nodesEdges
          nodesEdges = map show (graphNodes g) ++ map show (graphEdges g)

dirGraph :: String
dirGraph = "digraph"

undirGraph :: String
undirGraph = "graph"

-- -----------------------------------------------------------------------------

data GraphID = Str String
             | Num String
             | QStr String
             | HTML String

instance Show GraphID where
    show (Str str)  = str
    show (Num str)  = str
    show (QStr str) = '\"' : str ++ "\""
    show (HTML str) = '<' : str ++ ">"

readGraphID :: Parser Char GraphID
readGraphID = oneOf [ readStrID
                    , readNumID
                    , readQStrID
                    , readHtmlID
                    ]

readStrID :: Parser Char GraphID
readStrID = do frst <- satisfy frstCond
               rest <- many (satisfy restCond)
               return $ Str $ frst : rest
    where
      frstCond c = any ($c) [ isAsciiUpper
                            , isAsciiLower
                            , (==) '_'
                            , \ x -> x >= '\200' && x <= '\377'
                            ]

      restCond c = frstCond c || isDigit c

readNumID :: Parser Char GraphID
readNumID = do neg <- optional (char '-')
               num <- oneOf [nonInt, hasInt]
               return $ Num $ maybe id (:) neg num
    where
      nonInt = do d <- char '.'
                  digs <- many1 digit
                  return $ d : digs
      hasInt = do int <- many1 digit
                  dec <- optional $ do d <- char '.'
                                       digs <- many digit
                                       return $ d : digs
                  return $ maybe id (flip (++)) dec int

readQStrID :: Parser Char GraphID
readQStrID = do char qt
                cnt <- many1 (oneOf [ string eQt
                                    , liftM return $ satisfy (not . (==) qt)
                                    ] )
                char qt
                return $ QStr (concat cnt)
  where
    qt = '\"'
    eQt = "\\\""

readHtmlID :: Parser Char GraphID
readHtmlID = do char open
                cnt <- many1 $ satisfy ((/=) close)
                char close
                return $ HTML cnt
    where
      open = '<'
      close = '>'

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
    show = init . unlines . addTabs . nodesToString

nodesToString :: DotNode -> [String]
nodesToString n@(DotNode {})
    | null nAs  = [nID ++ ";"]
    | otherwise = [nID ++ (' ':(show nAs ++ ";"))]
    where
      nID = show $ nodeID n
      nAs = nodeAttributes n
nodesToString c@(DotCluster {})
    = ["subgraph cluster_" ++ clusterID c ++ " {"] ++ addTabs inner ++ ["}"]
    where
      inner = case clusterAttributes c of
                [] -> nodes
                a  -> ("graph " ++ show a ++ ";") : nodes
      nodes = concatMap nodesToString $ clusterElems c

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
    show e
        = '\t' : (show (edgeHeadNodeID e)
                  ++ edge ++ show (edgeTailNodeID e) ++ attributes)
          where
            edge = " " ++ (if directedEdge e then dirEdge else undirEdge) ++ " "
            attributes = case edgeAttributes e of
                           [] -> ";"
                           a  -> ' ':(show a ++ ";")

dirEdge :: String
dirEdge = "->"

undirEdge :: String
undirEdge = "--"

-- -----------------------------------------------------------------------------

-- | Parse a 'DotNode'
readDotNode :: Parser Char DotNode
readDotNode = do optional whitespace
                 nId <- number
                 as <- optional (whitespace >> readAttributesList)
                 char ';'
                 skipToNewline
                 return DotNode { nodeID = nId
                                , nodeAttributes = fromMaybe [] as }


-- | Parse a 'DotEdge'
readDotEdge :: Parser Char DotEdge
readDotEdge = do optional whitespace
                 eHead <- number
                 whitespace
                 edgeType <- strings [dirEdge,undirEdge]
                 whitespace
                 eTail <- number
                 as <- optional (whitespace >> readAttributesList)
                 char ';'
                 skipToNewline
                 return DotEdge { edgeHeadNodeID = eHead
                                , edgeTailNodeID = eTail
                                , edgeAttributes = fromMaybe [] as
                                , directedEdge = edgeType == dirEdge
                                }


-- | Parse a 'DotGraph'
readDotGraph :: Parser Char DotGraph
readDotGraph = do isStrict <- parseAndSpace $ hasString "strict"
                  gType <- strings [dirGraph,undirGraph]
                  gId <- optional (readGraphID `discard` whitespace)
                  whitespace
                  char '{'
                  skipToNewline
                  as <- liftM concat $
                        many (optional whitespace >>
                              oneOf [ (string "edge" >> skipToNewline >> return [])
                                    , (string "node" >> skipToNewline >> return [])
                                    , (string "graph" >> whitespace >> readAttributesList >>= \as -> skipToNewline >> return as)
                                    ]
                             )
                  ns <- many readDotNode
                  es <- many readDotEdge
                  char '}'
                  return DotGraph { strictGraph = isStrict
                                  , directedGraph = gType == dirGraph
                                  , graphID = gId
                                  , graphAttributes = as
                                  , graphNodes = ns
                                  , graphEdges = es
                                  }
