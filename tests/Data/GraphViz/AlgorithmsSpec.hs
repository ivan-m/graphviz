{- |
   Module      : Data.GraphViz.AlgorithmsSpec
   Description : Testing algorithms
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.AlgorithmsSpec (spec) where

import Data.GraphViz.Algorithms         (CanonicaliseOptions)
import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_canonicalise,
                                         prop_canonicaliseEdges,
                                         prop_canonicaliseNodes,
                                         prop_transitive, prop_transitiveNodes)
import Data.GraphViz.Types.Canonical    (DotGraph)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  prop "Canonicalisation should be idempotent"
       (prop_canonicalise :: CanonicaliseOptions -> DotGraph Int -> Bool)
  prop "Canonicalisation shouldn't change any nodes"
       (prop_canonicaliseNodes :: CanonicaliseOptions -> DotGraph Int -> Bool)
  prop "Canonicalisation shouldn't change any edges"
       (prop_canonicaliseEdges :: CanonicaliseOptions -> DotGraph Int -> Bool)
  prop "Transitive reduction should be idempotent"
       (prop_transitive :: CanonicaliseOptions -> DotGraph Int -> Bool)
  prop "Transitive reduction shouldn't change any nodes"
       (prop_transitiveNodes :: CanonicaliseOptions -> DotGraph Int -> Bool)
