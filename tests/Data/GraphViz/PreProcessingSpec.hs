{- |
   Module      : Data.GraphViz.PreProcessingSpec
   Description : Test pre-processing
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.PreProcessingSpec where

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_preProcessingID)
import Data.GraphViz.Types.Canonical    (DotGraph)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)

--------------------------------------------------------------------------------

spec :: Spec
spec = prop "Preprocessing doesn't change Dot code"
            (prop_preProcessingID :: DotGraph Int -> Bool)
