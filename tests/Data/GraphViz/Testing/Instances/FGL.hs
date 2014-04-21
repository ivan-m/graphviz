{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
   Module      : Data.GraphViz.Testing.Instances.FGL
   Description : 'Arbitrary' instances for FGL graphs.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the 'Arbitrary' instances for FGL 'DynGraph'
   graphs.  Note that this instance cannot be in
   "Data.GraphViz.Testing.Instances", as this instance requires the
   FlexibleInstances extension, which makes some of the other
   'Arbitrary' instances fail to type-check.
-}
module Data.GraphViz.Testing.Instances.FGL() where

import Test.QuickCheck

import Data.GraphViz.Internal.Util (uniq)

import Control.Monad              (liftM, liftM3)
import Data.Function              (on)
import Data.Graph.Inductive.Graph (Graph, delNode, mkGraph, nodes)
import Data.List                  (sortBy)

-- -----------------------------------------------------------------------------
-- Arbitrary instance for FGL graphs.

instance (Graph g, Arbitrary n, Arbitrary e) => Arbitrary (g n e) where
  arbitrary = do ns <- suchThat genNs (not . null)
                 let nGen = elements ns
                 lns <- mapM makeLNode ns
                 les <- liftM (sortBy (compare `on` toE)) . listOf
                        $ makeLEdge nGen
                 return $ mkGraph lns les
    where
      genNs = liftM uniq arbitrary
      toE (f,t,_) = (f,t)
      makeLNode n = liftM ((,) n) arbitrary
      makeLEdge nGen = liftM3 (,,) nGen nGen arbitrary

  shrink gr = case nodes gr of
                   -- Need to have at least 2 nodes before we delete one!
                   ns@(_:_:_) -> map (`delNode` gr) ns
                   _          -> []
