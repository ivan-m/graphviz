{- |
   Module      : Data.GraphViz.Testing.Properties
   Description : Properties for testing.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Various properties that should hold true for the graphviz library.
-}
module Data.GraphViz.Testing.Properties where

import Data.GraphViz(dotizeGraph', prettyPrint')
import Data.GraphViz.Types(DotRepr, DotGraph, printDotGraph)
import Data.GraphViz.Types.Generalised(generaliseDotGraph)
import Data.GraphViz.Printing(PrintDot(..), printIt)
import Data.GraphViz.Parsing(ParseDot(..), parseIt, parseIt', preProcess)
import Data.GraphViz.Util(groupSortBy, isSingle)

import Test.QuickCheck

import Data.Graph.Inductive(DynGraph, equal, nmap, emap, labEdges)
import Data.List(nub)

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
prop_generalisedSameDot    :: (ParseDot n, PrintDot n) => DotGraph n -> Bool
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

-- | This property verifies that 'dotizeGraph'', etc. only /augment/ the
--   original graph; that is, the actual nodes, edges and labels for
--   each remain unchanged.  Whilst 'dotize'', etc. only require
--   'Graph' instances, this property requires 'DynGraph' (which is a
--   sub-class of 'Graph') instances to be able to strip off the
--   'Attributes' augmentations.
prop_dotizeAugment   :: (DynGraph g, Eq n, Ord e) => g n e -> Bool
prop_dotizeAugment g = equal g (unAugment g')
  where
    g' = dotizeGraph' g
    unAugment = nmap snd . emap snd

-- | When a graph with multiple edges is augmented, then all edges
--   should have unique 'Attributes' (namely the positions).  Note
--   that this may not hold true with custom supplied 'Attributes'
--   (i.e. not using one of the @dotize@ functions).
prop_dotizeAugmentUniq   :: (DynGraph g, Eq n, Ord e) => g n e -> Bool
prop_dotizeAugmentUniq g = all uniqLs lss
  where
    g' = dotizeGraph' g
    les = map (\(f,t,l) -> ((f,t),l)) $ labEdges g'
    lss = map (map snd) . filter (not . isSingle)
          $ groupSortBy fst les
    uniqLs ls = ls == nub ls

-- -----------------------------------------------------------------------------
-- Helper utility functions

-- | A utility function to use for debugging purposes for trying to
--   find how graphviz /is/ parsing something.  This is easier than
--   using @'parseIt' . 'printIt'@ directly, since it avoids having to
--   enter and explicit type signature.
tryParse :: (ParseDot a, PrintDot a) => a -> (a, String)
tryParse = parseIt . printIt

-- | Equivalent to 'tryParse' except that it is assumed that the
--   entire 'String' *is* fully consumed.
tryParse' :: (ParseDot a, PrintDot a) => a -> a
tryParse' = parseIt' . printIt
