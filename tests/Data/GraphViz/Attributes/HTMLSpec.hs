{- |
   Module      : Data.GraphViz.Attributes.HTMLSpec
   Description : HTML label testing
   Copyright   : Ivan Lazar Miljenovic
   License     : BSD3
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is in addition to "Data.GraphViz.Attributes.CompleteSpec" as
   HTML labels are also likely to have their own quirks for testing.

 -}
module Data.GraphViz.Attributes.HTMLSpec (spec) where

import Data.GraphViz.Attributes.HTML    (Label)
import Data.GraphViz.Testing.Instances  ()
import Data.GraphViz.Testing.Properties (prop_printParseID)

import Test.Hspec            (Spec)
import Test.Hspec.QuickCheck (prop)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  prop "Printing and parsing of HTML labels"
       (prop_printParseID :: Label -> Bool)
