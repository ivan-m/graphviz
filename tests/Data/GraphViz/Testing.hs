{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, Rank2Types #-}

{- |
   Module      : Data.GraphViz.Testing
   Description : Test-suite for graphviz.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This defines a test-suite for the graphviz library.

   Limitations of the test suite are as follows:

   * For the most part, this library lets you use arbitrary numbers
     for String values.  However, this is not tested due to too many
     corner cases for special parsers that don't take arbitrary
     Strings.  As the Dot standard is ambiguous over whether you can
     or can't use numbers as Strings (more specifically, if they
     should be quoted or not), this is a user beware situation.

   * Same goes for empty Strings; sometimes they're allowed, sometimes
     they're not.  Thus, to simplify matters they're not generated.

   * The generated Strings are very simple, only composed of lower
     case letters, digits and some symbols.  This is because too many
     tests were \"failing\" due to some corner case; e.g. lower-case
     letters only because the parser parses Strings as lowercase, so
     if a particular String isn't valid (e.g. @\"all\"@ for 'LayerID',
     then the 'Arbitrary' instance has to ensure that all possible
     ways of capitalising that String isn't generated as a random
     'LRName'.

   * The generated 'DotRepr's are not guaranteed to be valid.

   * To avoid needless endless recursion, sub-graphs do not have their
     own internal sub-graphs.

   * This test suite isn't perfect: if you deliberately try to stuff
     something up, you probably can.
-}
module Data.GraphViz.Testing
       ( -- * Running the test suite.
         runChosenTests
       , runTests
       , runTest
         -- ** The tests themselves
       , Test(..)
       , defaultTests
       , allTests
       , test_printParseID_Attributes
       , test_generalisedSameDot
       , test_printParseID
       , test_preProcessingID
       , test_dotizeAugment
       , test_dotizeHasAugment
       , test_dotizeAugmentUniq
       , test_canonicalise
       , test_canonicaliseNodes
       , test_canonicaliseEdges
       , test_transitive
       , test_transitiveNodes
        -- * Re-exporting modules for manual testing.
       , module Data.GraphViz
       , module Data.GraphViz.Testing.Properties
         -- * Debugging printing
       , PrintDot(..)
       , printIt
       , renderDot
         -- * Debugging parsing
       , ParseDot(..)
       , parseIt
       , parseIt'
       , runParser
       , preProcess
       ) where

import Test.QuickCheck

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties

import           Data.GraphViz
import           Data.GraphViz.Algorithms        (CanonicaliseOptions)
import           Data.GraphViz.Parsing           (parseIt, parseIt', runParser)
import           Data.GraphViz.PreProcessing     (preProcess)
import           Data.GraphViz.Printing          (printIt, renderDot)
import qualified Data.GraphViz.Types.Generalised as G
import qualified Data.GraphViz.Types.Graph       as Gr
-- Can't use PatriciaTree because a Show instance is needed.
import Data.Graph.Inductive.Tree (Gr)

import System.Exit (ExitCode (..), exitWith)
import System.IO   (hPutStrLn, stderr)

-- -----------------------------------------------------------------------------

runChosenTests       :: [Test] -> IO ()
runChosenTests tsts = do putStrLn msg
                         blankLn
                         runTests tsts
                         spacerLn
                         putStrLn successMsg
  where
    msg = "This is the test suite for the graphviz library.\n\
           \If any of these tests fail, please inform the maintainer,\n\
           \including full output of this test suite."

    successMsg = "All tests were successful!"


-- -----------------------------------------------------------------------------
-- Defining a Test structure and how to run tests.

-- | Defines the test structure being used.
data Test = Test { name       :: String
                 , lookupName :: String      -- ^ Should be lowercase
                 , desc       :: String
                 , tests      :: [IO Result] -- ^ QuickCheck test.
                 }

-- | Run all of the provided tests.
runTests :: [Test] -> IO ()
runTests = mapM_ ((>>) spacerLn . runTest)

-- | Run the provided test.
runTest     :: Test -> IO ()
runTest tst = do putStrLn title
                 blankLn
                 putStrLn $ desc tst
                 blankLn
                 run $ tests tst
                 blankLn
  where
    nm = '"' : name tst ++ "\""
    title = "Running test: " ++ nm ++ "."
    successMsg = "All tests for " ++ nm ++ " were successful!"
    gaveUpMsg = "Too many sample inputs for " ++ nm ++ " were rejected;\n\
                 \tentatively marking this as successful."
    failMsg = "The tests for " ++ nm ++ " failed!\n\
               \Not attempting any further tests."

    run [] = putStrLn successMsg
    run (t:ts) = do r <- t
                    case r of
                       Success{} -> run ts
                       GaveUp{}  -> putStrLn gaveUpMsg >> run ts
                       _         -> die failMsg

spacerLn :: IO ()
spacerLn = putStrLn (replicate 70 '=') >> blankLn

blankLn :: IO ()
blankLn = putStrLn ""

die     :: String -> IO a
die msg = do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

qCheck :: (Testable prop) => prop -> IO Result
qCheck = quickCheckWithResult (stdArgs { maxSize = 50, maxSuccess = 200 })

-- -----------------------------------------------------------------------------
-- Defining the tests to use.

-- | The tests to run by default.
defaultTests :: [Test]
defaultTests = [ test_printParseID_Attributes
               , test_generalisedSameDot
               , test_printParseID
               , test_preProcessingID
               -- These require dot and neato to be installed and
               -- configured properly.  As such, don't run them by
               -- default.

               -- , test_dotizeAugment
               -- , test_dotizeHasAugment
               -- , test_dotizeAugmentUniq
               , test_findAllNodes
               , test_findAllNodesE
               , test_findAllEdges
               , test_noGraphInfo
               , test_canonicalise
               , test_canonicaliseNodes
               , test_canonicaliseEdges
               , test_transitive
               , test_transitiveNodes
               ]

-- | All available tests.
allTests :: [Test]
allTests = [ test_printParseID_Attributes
           , test_generalisedSameDot
           , test_printParseID
           , test_preProcessingID
           , test_dotizeAugment
           , test_dotizeHasAugment
           , test_dotizeAugmentUniq
           , test_findAllNodes
           , test_findAllNodesE
           , test_findAllEdges
           , test_noGraphInfo
           , test_canonicalise
           , test_canonicaliseNodes
           , test_canonicaliseEdges
           , test_transitive
           , test_transitiveNodes
           ]

-- | Test that 'Attributes' can be printed and then parsed back.
test_printParseID_Attributes :: Test
test_printParseID_Attributes
  = Test { name       = "Printing and parsing of Attributes"
         , lookupName = "attributes"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: Attributes -> Property
    prop = prop_printParseListID

    dsc = "The most common source of errors in printing and parsing are for\n\
          \Attributes."

test_generalisedSameDot :: Test
test_generalisedSameDot
  = Test { name       = "Printing generalised Dot code"
         , lookupName = "makegeneralised"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
    where
      prop :: DotGraph Int -> Bool
      prop = prop_generalisedSameDot

      dsc = "When generalising \"DotGraph\" values to other \"DotRepr\" values,\n\
             \the generated Dot code should be identical."

test_printParseID :: Test
test_printParseID
  = Test { name       = "Printing and Parsing DotReprs"
         , lookupName = "printparseid"
         , desc       = dsc
         , tests      = tsts
         }
  where
    tsts :: [IO Result]
    tsts = [ qCheck (prop_printParseID :: DotGraph    Int -> Bool)
           , qCheck (prop_printParseID :: G.DotGraph  Int -> Bool)
           , qCheck (prop_printParseID :: Gr.DotGraph Int -> Bool)
           ]

    dsc = "The graphviz library should be able to parse back in its own\n\
           \generated Dot code for any \"DotRepr\" instance"

test_preProcessingID :: Test
test_preProcessingID
  = Test { name       = "Pre-processing Dot code"
         , lookupName = "preprocessing"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: DotGraph Int -> Bool
    prop = prop_preProcessingID

    dsc = "When parsing Dot code, some pre-processing is done to remove items\n\
           \such as comments and to join together multi-line strings.  This\n\
           \test verifies that this pre-processing doesn't affect actual\n\
           \Dot code by running the pre-processor on generated Dot code.\n\n\
           \This test is not run on generalised Dot graphs as if it works for\n\
           \normal dot graphs then it should also work for generalised ones."

augMsg :: String
augMsg = "\n\nThis requires dot and neato to be installed, and for `dot' to be\n\
         \to be in the output of `dot -Txxx`."

test_dotizeAugment :: Test
test_dotizeAugment
  = Test { name       = "Augmenting FGL Graphs"
         , lookupName = "augment"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: Gr Char Double -> Bool
    prop = prop_dotizeAugment

    dsc = "The various Graph to Graph functions in Data.GraphViz should\n\
           \only _augment_ the graph labels and not change the graphs\n\
           \themselves.  This test compares the original graphs to these\n\
           \augmented graphs and verifies that they are the same." ++ augMsg

test_dotizeHasAugment :: Test
test_dotizeHasAugment
  = Test { name       = "Ensuring augmentation of FGL Graphs"
         , lookupName = "hasaugment"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: Gr Char Double -> Bool
    prop = prop_dotizeHasAugment

    dsc = "The various Graph to Graph functions in Data.GraphViz should\n\
           \actually agument the graph labels; this ensures that all labels\n\
           \actually have attached Attributes after augmentation." ++ augMsg

test_dotizeAugmentUniq :: Test
test_dotizeAugmentUniq
  = Test { name       = "Unique edges in augmented FGL Graphs"
         , lookupName = "augmentuniq"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: Gr Char Double -> Bool
    prop = prop_dotizeAugmentUniq

    dsc = "When augmenting a graph with multiple edges, as long as no\n\
           \Attributes are provided that override the default settings,\n\
           \then each edge between two nodes should have a unique position\n\
           \Attribute, etc." ++ augMsg

test_findAllNodes :: Test
test_findAllNodes
  = Test { name       = "Ensure all nodes are found in a DotRepr"
         , lookupName = "findnodes"
         , desc       = dsc
         , tests      = map qCheck props
         }
  where
    props :: [Gr () () -> Bool]
    props = testAllGraphTypes prop_findAllNodes

    dsc = "nodeInformation should find all nodes in a DotRepr;\n\
           \this is tested by converting an FGL graph and comparing\n\
           \the nodes it should have to those that are found."

test_findAllNodesE :: Test
test_findAllNodesE
  = Test { name       = "Ensure all nodes are found in a node-less DotRepr"
         , lookupName = "findedgelessnodes"
         , desc       = dsc
         , tests      = map qCheck props
         }
  where
    props :: [Gr () () -> Bool]
    props = testAllGraphTypes prop_findAllNodesE

    dsc = "nodeInformation should find all nodes in a DotRepr,\n\
           \even if there are no explicit nodes in that graph.\n\
           \This is tested by converting an FGL graph and comparing\n\
           \the nodes it should have to those that are found."

test_findAllEdges :: Test
test_findAllEdges
  = Test { name       = "Ensure all edges are found in a DotRepr"
         , lookupName = "findedges"
         , desc       = dsc
         , tests      = map qCheck props
         }
  where
    props :: [Gr () () -> Bool]
    props = testAllGraphTypes prop_findAllEdges

    dsc = "nodeInformation should find all edges in a DotRepr;\n\
           \this is tested by converting an FGL graph and comparing\n\
           \the edges it should have to those that are found."

test_noGraphInfo :: Test
test_noGraphInfo
  = Test { name       = "Plain DotReprs should have no structural information"
         , lookupName = "nographinfo"
         , desc       = dsc
         , tests      = map qCheck props
         }
  where
    props :: [Gr () () -> Bool]
    props = testAllGraphTypes prop_noGraphInfo

    dsc = "When converting a Graph to a DotRepr, there should be no\n\
           \clusters or global attributes."

test_canonicalise :: Test
test_canonicalise
  = Test { name       = "Canonicalisation should be idempotent"
         , lookupName = "canonicalise"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: CanonicaliseOptions -> DotGraph Int -> Bool
    prop = prop_canonicalise

    dsc = "Repeated application of canonicalise shouldn't have any further affect."

test_canonicaliseNodes :: Test
test_canonicaliseNodes
  = Test { name       = "Canonicalisation shouldn't change any nodes"
         , lookupName = "canonicalisenodes"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: CanonicaliseOptions -> DotGraph Int -> Bool
    prop = prop_canonicaliseNodes

    dsc = "Canonicalisation shouldn't change or remove any nodes."

test_canonicaliseEdges :: Test
test_canonicaliseEdges
  = Test { name       = "Canonicalisation shouldn't change any edges"
         , lookupName = "canonicaliseedges"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: CanonicaliseOptions -> DotGraph Int -> Bool
    prop = prop_canonicaliseEdges

    dsc = "Canonicalisation shouldn't change or remove any edges."

test_transitive :: Test
test_transitive
  = Test { name       = "Transitive reduction should be idempotent"
         , lookupName = "transitive"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: CanonicaliseOptions -> DotGraph Int -> Bool
    prop = prop_transitive

    dsc = "Repeated application of transitiveReduction shouldn't have any further affect."

test_transitiveNodes :: Test
test_transitiveNodes
  = Test { name       = "Transitive reduction shouldn't change any nodes"
         , lookupName = "transitivenodes"
         , desc       = dsc
         , tests      = [qCheck prop]
         }
  where
    prop :: CanonicaliseOptions -> DotGraph Int -> Bool
    prop = prop_transitiveNodes

    dsc = "Transitive reduction shouldn't change or remove any nodes."

-- -----------------------------------------------------------------------------

-- | Used when a property takes in a DotRepr as the first argument to
--   indicate which instance it should test via 'fromCanonical'.
testAllGraphTypes      :: (Testable prop)
                          => (forall dg. (Eq (dg Int), DotRepr dg Int) => dg Int -> prop)
                          -> [prop]
testAllGraphTypes prop = [ prop (undefined :: DotGraph Int)
                         , prop (undefined :: G.DotGraph Int)
                         , prop (undefined :: Gr.DotGraph Int)
                         ]
