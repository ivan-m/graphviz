{- |
   Module      : Data.GraphViz.Attributes.CompleteSpec
   Description : Attribute testing
   Copyright   : Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Data.GraphViz.Attributes.CompleteSpec (spec) where

import Data.GraphViz.Attributes.Complete (Attributes)
import Data.GraphViz.Testing.Instances   ()
import Data.GraphViz.Testing.Properties  (prop_printParseListID)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck       (Property)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  prop "Printing and parsing of attributes"
       (prop_printParseListID :: Attributes -> Property)
