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
                    , setDirectedness, nonClusteredParams, prettyPrint')
import Data.GraphViz.Types( DotRepr, DotGraph(..), DotStatements(..)
                          , DotNode(..), DotEdge(..), GlobalAttributes(..)
                          , printDotGraph, graphNodes, graphEdges
                          , graphStructureInformation)
import Data.GraphViz.Types.Generalised(generaliseDotGraph)
import Data.GraphViz.Printing(PrintDot(..), printIt)
import Data.GraphViz.Parsing(ParseDot(..), parseIt, parseIt')
import Data.GraphViz.PreProcessing(preProcess)
import Data.GraphViz.Util(groupSortBy, isSingle)

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

-- | When converting a 'DotGraph' value to a 'GDotGraph' one, they
--   should generate the same Dot code.
prop_generalisedSameDot    :: (Ord n, ParseDot n, PrintDot n)
                              => DotGraph n -> Bool
prop_generalisedSameDot dg = printDotGraph dg == printDotGraph gdg
  where
    gdg = generaliseDotGraph dg

-- | Pre-processing shouldn't change the output of printed Dot code.
--   This should work for all 'PrintDot' instances, but is more
--   specific to 'DotGraph' values.
prop_preProcessingID    :: (DotRepr dg n) => dg n -> Bool
prop_preProcessingID dg = preProcess dotCode == dotCode
  where
    dotCode = printDotGraph dg

-- | This is a version of 'prop_printParseID' that tries to parse the
--   pretty-printed output of 'prettyPrint'' rather than just 'printIt'.
prop_parsePrettyID    :: (DotRepr dg n, Eq (dg n), ParseDot (dg n))
                         => dg n -> Bool
prop_parsePrettyID dg = (parseIt' . prettyPrint') dg == dg

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

-- | Ensure that the definition of 'nodeInformation' for 'DotGraph'
--   finds all the nodes.
prop_findAllNodes   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllNodes g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg

-- | Ensure that the definition of 'nodeInformation' for 'GDotGraph'
--   finds all the nodes.
prop_findAllNodesG   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllNodesG g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = generaliseDotGraph $ setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg

-- | Ensure that the definition of 'nodeInformation' for 'DotGraph'
--   finds all the nodes when the explicit 'DotNode' definitions are
--   removed.
prop_findAllNodesE   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllNodesE g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = removeNodes $ setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg
    removeNodes dot@DotGraph{graphStatements = stmts}
      = dot { graphStatements
               = stmts {nodeStmts = filter notInEdge $ nodeStmts stmts}
            }
    gnes = Set.fromList . concatMap (\(f,t) -> [f,t]) $ edges g
    notInEdge dn = nodeID dn `Set.notMember` gnes

-- | Ensure that the definition of 'nodeInformation' for 'GDotGraph'
--   finds all the nodes when the explicit 'DotNode' definitions are
--   removed.
prop_findAllNodesEG   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllNodesEG g = ((==) `on` sort) gns dgns
  where
    gns = nodes g
    dg = generaliseDotGraph . removeNodes
         $ setDirectedness graphToDot nonClusteredParams g
    dgns = map nodeID $ graphNodes dg
    removeNodes dot@DotGraph{graphStatements = stmts}
      = dot { graphStatements
               = stmts {nodeStmts = filter notInEdge $ nodeStmts stmts}
            }
    gnes = Set.fromList . concatMap (\(f,t) -> [f,t]) $ edges g
    notInEdge dn = nodeID dn `Set.notMember` gnes

-- | Ensure that the definition of 'edgeInformation' for 'DotGraph'
--   finds all the nodes.
prop_findAllEdges   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllEdges g = ((==) `on` sort) ges dges
  where
    ges = edges g
    dg = graphToDot nonClusteredParams g
    dges = map (edgeFromNodeID &&& edgeToNodeID) $ graphEdges dg

-- | Ensure that the definition of 'edgeInformation' for 'GDotGraph'
--   finds all the nodes.
prop_findAllEdgesG   :: (Ord el, Graph g) => g nl el -> Bool
prop_findAllEdgesG g = ((==) `on` sort) ges dges
  where
    ges = edges g
    dg = generaliseDotGraph $ graphToDot nonClusteredParams g
    dges = map (edgeFromNodeID &&& edgeToNodeID) $ graphEdges dg

-- | There should be no clusters or global attributes when converting
--   a 'Graph' to a 'DotGraph' without any formatting or clustering.
prop_noGraphInfo   :: (Ord el, Graph g) => g nl el -> Bool
prop_noGraphInfo g = info == (GraphAttrs [], Map.empty)
  where
    dg = setDirectedness graphToDot nonClusteredParams g
    info = graphStructureInformation dg

-- | There should be no clusters or global attributes when converting
--   a 'Graph' to a 'GDotGraph' without any formatting or clustering.
prop_noGraphInfoG   :: (Ord el, Graph g) => g nl el -> Bool
prop_noGraphInfoG g = info == (GraphAttrs [], Map.empty)
  where
    dg = generaliseDotGraph $ setDirectedness graphToDot nonClusteredParams g
    info = graphStructureInformation dg

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
