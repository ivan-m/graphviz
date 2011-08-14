{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.Graph
   Description : Graph dot graph instances for Arbitrary.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com
 -}
module Data.GraphViz.Testing.Instances.Graph where

import Data.GraphViz.Testing.Instances.Canonical()

import Data.GraphViz.Types.Graph
import Data.GraphViz.Types(fromCanonical)

import Test.QuickCheck

import Control.Monad(liftM)

-- -----------------------------------------------------------------------------

-- | Can't directly create one of these as it might not match the
--   internal format directly; as such, have to use the inefficient
--   'fromCanonical' route.
instance (Arbitrary n, Ord n) => Arbitrary (DotGraph n) where
  arbitrary = liftM fromCanonical arbitrary

  shrink = map fromCanonical . shrink . toCanonical
