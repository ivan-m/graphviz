{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Common()
   Description : Attribute instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Common
       ( gaGraph
       , gaSubGraph
       , gaClusters
       ) where

import Data.GraphViz.Testing.Instances.Attributes
import Data.GraphViz.Testing.Instances.Helpers

import Data.GraphViz.Algorithms            (CanonicaliseOptions (..))
import Data.GraphViz.Attributes            (Attributes)
import Data.GraphViz.Types.Internal.Common (DotEdge (..), DotNode (..),
                                            GlobalAttributes (..), GraphID (..))

import Test.QuickCheck

import Control.Monad (liftM, liftM2, liftM3)

-- -----------------------------------------------------------------------------
-- Common values

instance Arbitrary GraphID where
  arbitrary = oneof [ liftM Str arbitrary
                    , liftM Num arbitrary
                    ]

  shrink (Str s) = map Str $ shrink s
  shrink (Num n) = map Num $ shrink n

instance (Arbitrary n) => Arbitrary (DotNode n) where
  arbitrary = liftM2 DotNode arbitrary arbNodeAttrs

  shrink (DotNode n as) = map (DotNode n) $ shrink as

instance (Arbitrary n) => Arbitrary (DotEdge n) where
  arbitrary = liftM3 DotEdge arbitrary arbitrary arbEdgeAttrs

  shrink (DotEdge f t as) = map (DotEdge f t) $ shrink as

instance Arbitrary GlobalAttributes where
  arbitrary = gaGraph

  shrink (GraphAttrs atts) = map GraphAttrs $ nonEmptyShrinks atts
  shrink (NodeAttrs  atts) = map NodeAttrs  $ nonEmptyShrinks atts
  shrink (EdgeAttrs  atts) = map EdgeAttrs  $ nonEmptyShrinks atts

gaGraph :: Gen GlobalAttributes
gaGraph = gaFor arbGraphAttrs

gaSubGraph :: Gen GlobalAttributes
gaSubGraph = gaFor arbSubGraphAttrs

gaClusters :: Gen GlobalAttributes
gaClusters = gaFor arbClusterAttrs

gaFor   :: Gen Attributes -> Gen GlobalAttributes
gaFor g = oneof [ liftM GraphAttrs g
                , liftM NodeAttrs  arbNodeAttrs
                , liftM EdgeAttrs  arbEdgeAttrs
                ]

instance Arbitrary CanonicaliseOptions where
  arbitrary = liftM2 COpts arbitrary arbitrary
