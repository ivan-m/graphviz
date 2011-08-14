{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

{- |
   Module      : Data.GraphViz.Testing.Properties
   Description : Properties for testing.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Various properties that should hold true for the graphviz library.
-}
module Data.GraphViz.Testing.Properties where

import Data.GraphViz( dotizeGraph, graphToDot
                    , setDirectedness, nonClusteredParams)
import Data.GraphViz.Types( DotRepr(..), PrintDotRepr, ParseDotRepr, PPDotRepr
                          , DotNode(..), DotEdge(..), GlobalAttributes(..)
                          , printDotGraph, graphNodes, graphEdges
                          , graphStructureInformation)
import Data.GraphViz.Types.Canonical(DotGraph(..), DotStatements(..))
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Printing(PrintDot(..), printIt)
import Data.GraphViz.Parsing(ParseDot(..), parseIt, parseIt')
import Data.GraphViz.PreProcessing(preProcess)
import Data.GraphViz.Util(groupSortBy, isSingle)
import Data.GraphViz.Algorithms

import Test.QuickCheck

import Data.Graph.Inductive( Graph, DynGraph
                           , equal, nmap, emap, labEdges, nodes, edges)
import Data.List(nub, sort)
import Data.Function(on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Lazy(Text)
import Control.Arrow((&&&))

-- -----------------------------------------------------------------------------
-- The properties to test for

-- | Checking that @parse . print == id@; that is, graphviz can parse
--   its own output.
prop_printParseID   :: (ParseDot a, PrintDot a, Eq a) => a -> Bool
prop_printParseID a = tryParse' a == a

-- | A version of 'prop_printParse' specifically for lists; it ensures
--   that the list is not empty (as most list-based parsers fail on
--   empty lists).
prop_printParseListID    :: (ParseDot a, PrintDot a, Eq a) => [a] -> Property
prop_printParseListID as =  not (null as) ==> prop_printParseID as

-- | When converting a canonical 'DotGraph' value to any other one,
--   they should generate the same Dot code.
prop_generalisedSameDot    :: (Ord n, PrintDot n, ParseDot n) => DotGraph n -> Bool
prop_generalisedSameDot dg = printDotGraph dg == printDotGraph gdg
  where
    gdg = canonicalToType (undefined :: G.DotGraph n) dg

-- | Pre-processing shouldn't change the output of printed Dot code.
--   This should work for all 'PrintDot' instances, but is more
--   specific to 'DotGraph' values.
prop_preProcessingID    :: (PrintDotRepr dg n) => dg n -> Bool
prop_preProcessingID dg = preProcess dotCode == dotCode
  where
    dotCode = printDotGraph dg

-- | This property verifies that 'dotizeGraph', etc. only /augment/ the
--   original graph; that is, the actual nodes, edges and labels for
--   each remain unchanged.  Whilst 'dotize', etc. only require
--   'Graph' instances, this property requires 'DynGraph' (which is a
--   sub-class of 'Graph') instances to be able to strip off the
--   'Attributes' augmentations.
prop_dotizeAugment   :: (DynGraph g, Eq n, Ord e) => g n e -> Bool
prop_dotizeAugment g = equal g (unAugment g')
  where
    g' = setDirectedness dotizeGraph nonClusteredParams g
    unAugment = nmap snd . emap snd

-- | When a graph with multiple edges is augmented, then all edges
--   should have unique 'Attributes' (namely the positions).  Note
--   that this may not hold true with custom supplied 'Attributes'
--   (i.e. not using one of the @dotize@ functions).
prop_dotizeAugmentUniq   :: (DynGraph g, Eq n, Ord e) => g n e -> Bool
prop_dotizeAugmentUniq g = all uniqLs lss
  where
    g' = setDirectedness dotizeGraph nonClusteredParams g
    les = map (\(f,t,l) -> ((f,t),l)) $ labEdges g'
    lss = map (map snd) . filter (not . isSingle)
          $ groupSortBy fst les
    uniqLs ls = ls == nub ls

-- | Ensure that the definition of 'nodeInformation' for a DotRepr
--   finds all the nodes.
prop_findAllNodes       :: (DotRepr dg Int, Ord el, Graph g)
                           => dg Int -> g nl el -> Bool
prop_findAllNodes dg' g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = canonicalToType dg' $ setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg

-- | Ensure that the definition of 'nodeInformation' for DotReprs
--   finds all the nodes when the explicit 'DotNode' definitions are
--   removed.
prop_findAllNodesE       :: (DotRepr dg Int, Ord el, Graph g)
                            => dg Int -> g nl el -> Bool
prop_findAllNodesE dg' g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = canonicalToType dg' . removeNodes $ setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg
    removeNodes dot@DotGraph{graphStatements = stmts}
      = dot { graphStatements
               = stmts {nodeStmts = filter notInEdge $ nodeStmts stmts}
            }
    gnes = Set.fromList . concatMap (\(f,t) -> [f,t]) $ edges g
    notInEdge dn = nodeID dn `Set.notMember` gnes

-- | Ensure that the definition of 'edgeInformation' for DotReprs
--   finds all the nodes.
prop_findAllEdges       :: (DotRepr dg Int, Ord el, Graph g) => dg Int -> g nl el -> Bool
prop_findAllEdges dg' g = ((==) `on` sort) ges dges
  where
    ges = edges g
    dg = canonicalToType dg' $ graphToDot nonClusteredParams g
    dges = map (fromNode &&& toNode) $ graphEdges dg

-- | There should be no clusters or global attributes when converting
--   a 'Graph' to a DotRepr (via fromCanonical) without any formatting
--   or clustering.
prop_noGraphInfo       :: (DotRepr dg Int, Ord el, Graph g)
                          => dg Int -> g nl el -> Bool
prop_noGraphInfo dg' g = info == (GraphAttrs [], Map.empty)
  where
    dg = canonicalToType dg'
         $ setDirectedness graphToDot nonClusteredParams g
    info = graphStructureInformation dg

-- | Canonicalisation should be idempotent.
prop_canonicalise   :: (ParseDot n, PrintDot n, DotRepr dg n) => dg n -> Bool
prop_canonicalise g = cdg == canonicalise cdg
  where
    cdg = canonicalise g

-- | Removing transitive edges should be idempotent.
prop_transitive   :: (DotRepr dg n) => dg n -> Bool
prop_transitive g = tdg == transitiveReduction tdg
  where
    tdg = transitiveReduction g

-- -----------------------------------------------------------------------------
-- Helper utility functions

-- | A utility function to use for debugging purposes for trying to
--   find how graphviz /is/ parsing something.  This is easier than
--   using @'parseIt' . 'printIt'@ directly, since it avoids having to
--   enter and explicit type signature.
tryParse :: (ParseDot a, PrintDot a) => a -> (a, Text)
tryParse = parseIt . printIt

-- | Equivalent to 'tryParse' except that it is assumed that the
--   entire 'String' *is* fully consumed.
tryParse' :: (ParseDot a, PrintDot a) => a -> a
tryParse' = parseIt' . printIt

-- | A wrapper around 'fromCanonical' that lets you specify up-front
--   what type to create (it need not be a sensible value).
canonicalToType   :: (DotRepr dg n) => dg n -> DotGraph n -> dg n
canonicalToType _ = fromCanonical
