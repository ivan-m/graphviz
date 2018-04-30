{- |
   Module      : Data.GraphVizSpec
   Description : Testing algorithms
   Copyright   : Matthew Sackman, Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphVizSpec (spec) where

import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_dotizeAugment,
                                         prop_dotizeAugmentUniq,
                                         prop_dotizeHasAugment)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)

import Data.Graph.Inductive.PatriciaTree (Gr)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  prop "FGL Graphs are augmentable"
       (prop_dotizeAugment :: GrType -> Bool)
  prop "Ensure augmentation is valid"
       (prop_dotizeHasAugment :: GrType -> Bool)
  prop "Unique edges in augmented FGL Graphs"
       (prop_dotizeAugmentUniq :: GrType -> Bool)

type GrType = Gr Char Double
