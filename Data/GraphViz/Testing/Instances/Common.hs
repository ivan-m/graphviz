{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Common()
   Description : Attribute instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Common() where

import Data.GraphViz.Testing.Instances.Attributes()
import Data.GraphViz.Testing.Instances.Helpers

import Data.GraphViz.Types.Common( DotNode(..), DotEdge(..)
                                 , GlobalAttributes(..), GraphID(..))

import Test.QuickCheck

import Control.Monad(liftM, liftM2, liftM3)

-- -----------------------------------------------------------------------------
-- Common values

instance Arbitrary GraphID where
  arbitrary = oneof [ liftM Str arbitrary
                    , liftM Int arbitrary
                    , liftM Dbl $ suchThat arbitrary notInt
                    ]

  shrink (Str s) = map Str $ shrink s
  shrink (Int i) = map Int $ shrink i
  shrink (Dbl d) = map Dbl $ filter notInt $ shrink d

instance (Arbitrary n) => Arbitrary (DotNode n) where
  arbitrary = liftM2 DotNode arbitrary arbitrary

  shrink (DotNode n as) = map (DotNode n) $ shrinkList as

instance (Arbitrary n) => Arbitrary (DotEdge n) where
  arbitrary = liftM3 DotEdge arbitrary arbitrary arbitrary

  shrink (DotEdge f t as) = map (DotEdge f t) $ shrinkList as

instance Arbitrary GlobalAttributes where
  arbitrary = oneof [ liftM GraphAttrs arbList
                    , liftM NodeAttrs  arbList
                    , liftM EdgeAttrs  arbList
                    ]

  shrink (GraphAttrs atts) = map GraphAttrs $ nonEmptyShrinks atts
  shrink (NodeAttrs  atts) = map NodeAttrs  $ nonEmptyShrinks atts
  shrink (EdgeAttrs  atts) = map EdgeAttrs  $ nonEmptyShrinks atts
